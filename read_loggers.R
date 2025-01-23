# read in Temperature and Humidity loggers

utils::download.file("https://filedn.eu/lKw35gljYV2BIxxlGg9SUJb/data/weather_loggers.zip",
                     destfile = "data/weather_loggers.zip")
utils::unzip(zipfile = "data/weather_loggers.zip",
              exdir = "data/")
unlink("data/weather_loggers.zip")

read_logger <- function(f_path, verbose = FALSE, lon = NA, lat = NA, ...){
   dat <-
      utils::read.csv(file = f_path,
                      stringsAsFactors = FALSE,
                      skip = 2,...)
   header <- readLines(f_path,n = 2)
   header[1] <- gsub("(\xb0C)","",header[1],useBytes = TRUE)

   if(grepl("High Alarm",header[1])){
      dat <- dat[,-4]
   }
   if(grepl("Low Alarm",header[1])){
      dat <- dat[,-4]
   }

   colnames(dat) <- c("index","date_time","Tm","RH","dew_point")

   if(verbose == TRUE){
      cat("\nlogger name: ", strsplit(header[1], split = ",")[[1]][1])
      cat("\nlogger ID: ", tail(strsplit(header[[2]], split = ",")[[1]],n = 1L))
      cat("\nstart: ", min(dat$date_time))
      cat("\nend: ", max(dat$date_time),"\n")
   }
   dat$name <- strsplit(header[1], split = ",")[[1]][1]
   dat$serial_n <- tail(strsplit(header[[2]], split = ",")[[1]],n = 1L)
   if(length(lon) >1 | length(lat) >1) stop("lon and lat must be a single value")
   dat$lon <- lon
   dat$lat <- lat

   return(dat[,c("name","serial_n","lon","lat","date_time","Tm","RH","dew_point")])
}

lggrs <- list.files("data/weather_loggers",pattern = ".txt", full.names = TRUE)

# read_logger(lggrs[1],verbose = TRUE)[1:5,]

library(data.table)

#details in blue notebook 24/11/2023
fwrite(read_logger(grep("231013_UQ-AGFS-10",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.939128,
                   lon = 153.191274),
       "cache/loggers/230915_Tamborine_010.csv")

fwrite(read_logger(grep("231013_UQ-AGFS-6_row49",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938859,
                   lon = 153.189725),
       "cache/loggers/230914_Tamborine_006.csv")
fwrite(read_logger(grep("231013_UQ-AGFS-7_row33N",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938518,
                   lon = 153.190244),
       "cache/loggers/230914_Tamborine_007.csv")
fwrite(read_logger(grep("231013_UQ-AGFS-8_row33S",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938946,
                   lon = 153.190178),
       "cache/loggers/230914_Tamborine_008.csv")
fwrite(read_logger(grep("231113_UQ_AGFS_10_Esky",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/23_Tamborine_0010.csv") # Esky
fwrite(read_logger(grep("231208-240108_UQ-AGFS-006",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938859,
                   lon = 153.189725),
       "cache/loggers/231208_Tamborine_006.csv") # Unknown location
fwrite(read_logger(grep("231208-240129_UQ-AGFS-007",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938518,
                   lon = 153.190244),
       "cache/loggers/231208_Tamborine_007.csv") # Unknown location
fwrite(read_logger(grep("231208-240331_UQ-AGFS-001",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_001.csv") # Unknown location
fwrite(read_logger(grep("231208-240331_UQ-AGFS-003",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_003.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-001",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/230914_Tamborine_001.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-003",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/230914_Tamborine_003.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-004",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/230914_Tamborine_004.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-005",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/230914_Tamborine_005.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-006",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231013_Tamborine_006.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-007",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231013_Tamborine_007.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-008",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231013_Tamborine_008.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ-AGFS-009",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231013_Tamborine_009.csv") # Unknown location
fwrite(read_logger(grep("231208_UQ_AGFS_10",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231110_Tamborine_010.csv") # Unknown location
fwrite(read_logger(grep("2404_UQ-AGFS-004",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_004.csv") # Unknown location
fwrite(read_logger(grep("2404_UQ-AGFS-005",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_005.csv") # Unknown location
fwrite(read_logger(grep("2404_UQ-AGFS-008",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_008.csv") # Unknown location
fwrite(read_logger(grep("2404_UQ-AGFS-009",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_009.csv") # Unknown location
fwrite(read_logger(grep("2404_UQ_AGFS_10",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = NA_real_,
                   lon = NA_real_),
       "cache/loggers/231208_Tamborine_010.csv") # Unknown location


######### 2024


fwrite(read_logger(grep("2409-2412_UQ-AGFS-004_row15N",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938658,
                   lon = 153.191025),
       "cache/loggers/240912_Tamborine_004.csv") # Unknown location
fwrite(read_logger(grep("2409-2412_UQ-AGFS-005_row32N",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938522,
                   lon = 153.190278),
       "cache/loggers/240912_Tamborine_005.csv") # Unknown location
fwrite(read_logger(grep("2409-2412_UQ-AGFS-008",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938569,
                   lon = 153.189933),
       "cache/loggers/240912_Tamborine_008.csv") # Unknown location
fwrite(read_logger(grep("2409-2412_UQ_AGFS_002_row15S",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.939012,
                   lon = 153.190957),
       "cache/loggers/240912_Tamborine_002.csv") # Unknown location
fwrite(read_logger(grep("2409-2412_UQ_AGFS_10_row57S",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938782,
                   lon = 153.189494),
       "cache/loggers/240912_Tamborine_010.csv") # Unknown location
fwrite(read_logger(grep("2412_UQ-AGFS-007_row43S",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938845,
                   lon = 153.189911),
       "cache/loggers/240912_Tamborine_007.csv") # Unknown location
fwrite(read_logger(grep("2412_UQ_AGFS_009_row52N",
                        lggrs,
                        value = TRUE),verbose = TRUE,
                   lat = -27.938440,
                   lon = 153.189519),
       "cache/loggers/240912_Tamborine_009.csv") # Unknown location











