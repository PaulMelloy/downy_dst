## ----------------------        Install packages        ----------------------
pkgs <- c("remotes",
          "data.table",
          "here",
          "ggplot2")
lapply(pkgs, function(p){
   if(p %in% installed.packages()[,"Package"] == FALSE){
      install.packages(p, dependencies = TRUE)
   }
})

if("data.table" %in% installed.packages()[,"Package"] == FALSE){
   install.packages("data.table",dependencies = TRUE)
}
if("epiphytoolR" %in% installed.packages()[,"Package"] == FALSE){
   remotes::install_github("PaulMelloy/epiphytoolR", ref = "fill_weather",
                           dependencies = TRUE)
}
if("viticolaR" %in% installed.packages()[,"Package"] == FALSE){
   remotes::install_github("PaulMelloy/viticolaR", ref = "dev",
                           dependencies = TRUE)
}

working_dir <- path.expand("~") # default to users home directory

library(data.table)
library(here)
library(ggplot2)
library(epiphytoolR)
library(viticolaR)
source(here("R/imp_bomstation_data.R"))


## ----------------------            Settings            ----------------------
# Machine specific setup
#source(here("R/run_on_pepper.R"))
dl_path <- file.path(working_dir,"weather_data/tgz/")
weather_path <- file.path(working_dir,"weather_data")
plots_path <- file.path(working_dir,"../shared")
shiny_img_path <- here("viticolR_dst/www")


## ---------------                System checks                ----------------
if(dir.exists(weather_path) == FALSE)
   stop("Path to weather data", weather_path, "does not exist")
# if (dir.exists(dl_path) == FALSE)
#    stop("Path to archived weather data", dl_path, "does not exist")
if (dir.exists(plots_path) == FALSE)
   stop("Path to save model plots", plots_path, "does not exist")
if (dir.exists(shiny_img_path) == FALSE)
   stop("Path to shiny images path", shiny_img_path, "does not exist"
   )


## --------------        Read in and format weather data       ----------------
# Get filepaths of merged weather data
weather_files <- list.files(weather_path,
                            pattern = ".csv",
                            full.names = TRUE)

# read in and impute missing weather data and format it for model
weather_list <- lapply(weather_files,
                       FUN = imp_bomstation_data,
                       variables = c("temp","rh","rain"),
                       lon = 153.1914,
                       lat = -27.9396,
                       rolling_window = 60,
                       min_wd_sd = 50,
                       rainNA = 0)

# re-save weather data without full names to get station names
weather_files <- list.files(weather_path,pattern = ".csv")
names(weather_list) <- tools::file_path_sans_ext(weather_files)


## --------------               Run viticolR model             ----------------
# run model on all weather stations
DMod_list <- lapply(weather_list,
                    function(sta){
                       message("Estimating downy primary inoculum for ",unique(sta$station))
                       viticolaR::estimate_DM_PI(sta)
                    })


## -----------------               Create plots             -------------------
names(DMod_list) <- names(weather_list)
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
         # Exclude NAs to avoid out of scale limit warnings
         geom_vline(xintercept = stats::na.exclude(s_mod[primary_infection_stage == "ZRE_ind",hour]),
                   colour = "#efc6c6")+
         geom_vline(xintercept = stats::na.exclude(s_mod[primary_infection_stage == "ZIN_ind",hour]),
                    colour = "darkred")+
         scale_x_continuous(breaks = seq(min(mod$time_hours),
                                         max(mod$time_hours),
                                         by = 60*60*24*2))+
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


   plot_width <- ifelse(length(mod$time_hours) < 1000,
                        1700,
                        length(mod$time_hours)*1.5)
   plot_filename <- paste0(loc,"_PI_SPO_plot.png")

   ggplot2::ggsave(filename = file.path(plots_path,plot_filename),
                   width = plot_width,
                   height = 1000,
                   units = "px")
   mod[["PI_SPO_plot"]] <- plot_filename

   return(mod)
})

## -----------------            Save all outputs            -------------------
names(DMod_list) <- tools::file_path_sans_ext(weather_files)

cat("Copy and overwrite files, reply with a TRUE seven times\n")
file.copy(from = list.files(plots_path,
                            pattern = ".png",
                            full.names = TRUE),
          to = shiny_img_path,
          overwrite = TRUE)

save(DMod_list,
     weather_list,
     file = file.path(working_dir,"../shared/","DM_dst_data.rda"))



