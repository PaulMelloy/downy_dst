library(data.table)
library(epiphytoolR)
source("/homevol/pmelloy/R/BarleyNetBlotchMeta/R/merge_axf_weather.R")

dl_time <- format(Sys.time(), format = "%y%m%d_%H%M")

Sys.sleep(round(runif(1,0,240))) # wait randomly between 0 amd 4 minutes

get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "QLD",
                     file_prefix = dl_time
                     )
Sys.sleep(round(runif(1,0,240)))
get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "NSW",
                     file_prefix = dl_time
)
Sys.sleep(round(runif(1,0,240)))# wait randomly between 0 amd 4 minutes
get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "NT"
)
Sys.sleep(round(runif(1,0,240)))# wait randomly between 0 amd 4 minutes
get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "SA",
                     file_prefix = dl_time
)
Sys.sleep(round(runif(1,0,240)))# wait randomly between 0 amd 4 minutes
get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "VIC"
)
Sys.sleep(round(runif(1,0,240)))# wait randomly between 0 amd 4 minutes
get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "WA"
)
Sys.sleep(round(runif(1,0,240)))# wait randomly between 0 amd 4 minutes
get_bom_observations(ftp_url = "ftp://ftp.bom.gov.au/anon/gen/fwo/",
                     download_location = "/homevol/pmelloy/Weather observations/tgz/",
                     access_warning = FALSE,
                     state = "TAS",
                     file_prefix = dl_time
)



# create folder to extract compressed file
dir_loc <- paste0("/homevol/pmelloy/Weather observations/", dl_time, "/")
dir.create(dir_loc)

# # uncompress file
# untar(tarfile = paste0("/homevol/pmelloy/Weather observations/tgz/",dl_time,"_IDQ60910.tgz"),
#       exdir = dir_loc)


# North Tamborine: IDQ60910.99123.axf
merge_axf_weather(File_compressed = paste0("/homevol/pmelloy/Weather observations/tgz/",dl_time,"_IDQ60910.tgz"),
                  File_axf = "IDQ60910.99123.axf",
                  File_formatted = "NTamborine.csv",
                  base_dir = "/homevol/pmelloy/Weather observations/"
                  )
source("~/R/preformat_Ntamborine_data.R")

# Gatton IDQ60910.94562.axf
merge_axf_weather(File_compressed = paste0("/homevol/pmelloy/Weather observations/tgz/",dl_time,"_IDQ60910.tgz"),
                  File_axf = "IDQ60910.94562.axf",
                  File_formatted = "Gatton_weather_obs.csv",
                  base_dir = "/homevol/pmelloy/Weather observations/"
)

