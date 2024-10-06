## -----------------        Install packages        -----------------
if("remotes" %in% installed.packages()[,"Package"] == FALSE){
   install.packages("remotes")
}
if("data.table" %in% installed.packages()[,"Package"] == FALSE){
   install.packages("data.table")
}
if("epiphytoolR" %in% installed.packages()[,"Package"] == FALSE){
   if(isFALSE(dir.exists("~/lib/R"))) dir.create("~/lib/R",recursive = TRUE)
   remotes::install_github("PaulMelloy/epiphytoolR", ref = "dev",
                           lib = "~/lib/R")
}
if("viticolaR" %in% installed.packages()[,"Package"] == FALSE){
   if(isFALSE(dir.exists("~/lib/R"))) dir.create("~/lib/R",recursive = TRUE)
   remotes::install_github("PaulMelloy/viticolaR", ref = "dev",
                           lib = "~/lib/R")
}

working_dir <- path.expand("~/") # default to users home directory

library(data.table)
library(ggplot2)
library(epiphytoolR)
library(viticolaR)
source("~/downy_dst/R/imp_bomstation_data.R")

## -----------------        Settings        -----------------
dl_path <- paste0(working_dir,"weather_data/tgz/")
weather_path <- paste0(working_dir,"weather_data")

## ----------            System checks            -----------
if(dir.exists(weather_path) == FALSE) stop("Path to weather data",
                                           weather_path,
                                           "does not exist")

# ## ----------          Merge weather data         -----------
# # Un-compress select stations and merge the data into one file for downstream
# #  Processing
#
# # North Tamborine: IDQ60910.99123.axf
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDQ60910.tgz"),
#                   File_axf = "IDQ60910.99123.axf",
#                   File_formatted = "23-24_NTamborine.csv",
#                   base_dir = weather_path
# )
#
# # Gatton IDQ60910.94562.axf
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDQ60910.tgz"),
#                   File_axf = "IDQ60910.94562.axf",
#                   File_formatted = "23-24_Gatton_weather_obs.csv",
#                   base_dir = weather_path
# )
#
# # Create weather for Applethorpe
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDQ60910.tgz"),
#                   File_axf = "IDQ60910.94553.axf",
#                   File_formatted = "23-24_Applethorpe.csv",
#                   base_dir = weather_path
# )
#
# # Update weather for Mildura Airport
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDV60910.tgz"),
#                   File_axf = "IDV60910.94693.axf",
#                   File_formatted = "23-24_MilduraAP.csv",
#                   base_dir = weather_path
# )
#
# # Update weather for Walpeup Research station.csv
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDV60910.tgz"),
#                   File_axf = "IDV60910.95831.axf",
#                   File_formatted = "23-24_WalpeupResearch.csv",
#                   base_dir = weather_path
# )
#
# # Update weather for Renmark  station.csv
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDS60910.tgz"),
#                   File_axf = "IDS60910.95687.axf",
#                   File_formatted = "23-24_RenmarkAP.csv",
#                   base_dir = weather_path
# )
#
# # Update weather for loxton station.csv
# merge_axf_weather(File_compressed = paste0(dl_path,dl_time,"_IDS60910.tgz"),
#                   File_axf = "IDS60910.94682.axf",
#                   File_formatted = "23-24_LoxtonResearch.csv",
#                   base_dir = weather_path
# )

weather_files <- list.files(weather_path,pattern = ".csv",
                            full.names = TRUE)

weather_list <- lapply(weather_files,
                       FUN = imp_bomstation_data,
                       variables = c("temp","rh","rain"),
                       lon = 153.1914,
                       lat = -27.9396,
                       rolling_window = 60,
                       min_wd_sd = 50,
                       rainNA = 0)

weather_files <- list.files(weather_path,pattern = ".csv")

names(weather_list) <- tools::file_path_sans_ext(weather_files)

DMod_list <- lapply(weather_list,
                    FUN = viticolaR::estimate_DM_PI)

##### testout <- viticolaR::estimate_DM_PI(weather_list[6])

DMod_list <- lapply(names(DMod_list),function(loc){
   # get model
   mod <- DMod_list[[loc]]

   s_mod <- get_PI_dates(mod)
   s_mod$primary_infection_stage <- as.character(s_mod$primary_infection_stage)

   p_out <-
   ggplot() +
      geom_ribbon_viticolaR(mod)+
      geom_line_viticolaR(mod)+
      scale_fill_gradient(name = "Mature Sporangia\ncohorts",
                          low = "#EBE9CF",
                          high = "#ADA205")+
      scale_color_continuous(name = "Immature sporangia\ncohorts")+
      theme_minimal()+
      coord_cartesian(ylim = c(0,1.2))+
      ylab("Progress towards sporangia maturity")+
      theme(legend.position="bottom",plot.background = element_rect(fill = "white"))+
      geom_rect(aes(xmin = head(mod$time_hours,n = 1),
                    xmax = tail(mod$time_hours,n = 1),
                    ymin = 1,
                    ymax = 2),
                fill = "grey",
                alpha = 0.6)+
      geom_vline(xintercept = s_mod[primary_infection_stage == "ZRE_ind",hour],
                 colour = "#efc6c6")+
      geom_vline(xintercept = s_mod[primary_infection_stage == "ZIN_ind",hour],
                 colour = "darkred")+
      scale_x_continuous(breaks = seq(min(mod$time_hours),
                                      max(mod$time_hours),
                                      by = 60*60*24*2))+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


   plot_width <- ifelse(length(mod$time_hours) < 1000,
                        1700,
                        length(mod$time_hours)*1.5)
   plot_filename <- paste0(loc,"_PI_SPO_plot.png")

   ggplot2::ggsave(filename = paste0("/home/shared/",plot_filename),
                   width = plot_width,
                   height = 1000,
                   units = "px")
   mod[["PI_SPO_plot"]] <- plot_filename

   return(mod)
})

names(DMod_list) <- tools::file_path_sans_ext(weather_files)
file.copy(from = list.files("/home/shared/",pattern = ".png", full.names = TRUE),
          to = "/home/pmelloy/shiny-server/viticolR_dst/www",
          overwrite = TRUE)

save(DMod_list,
     weather_list,
     file = paste0("/home/shared/","DM_dst_data.rda"))

# ----------------------------
# Run Historically
# ----------------------------
# # # find files with desired weather files
# zipfiles <- list.files(dl_path,pattern = "_IDS60910.tgz")
#
# for(i in zipfiles){
#    # Create weather for Applethorpe
#    #  Stanthorpe only records 3 hourly
#    merge_axf_weather(File_compressed = paste0(dl_path,i),
#                      File_axf = "IDS60910.94682.axf",
#                      File_formatted = "23-24_LoxtonResearch.csv",
#                      base_dir = weather_path,
#                      verbose = TRUE
#    )
#
#  }
#    merge_axf_weather(File_compressed = "/homevol/pmelloy/Weather observations/tgz/230825_1513_IDW60910.tgz",
#                      File_axf = "IDW60910.95641.axf",
#                      File_formatted = "23-24_error5.csv",
#                      base_dir = weather_path,
#                      verbose = TRUE
#    )


