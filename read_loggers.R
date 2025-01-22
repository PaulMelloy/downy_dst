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
                      skip = 2,
                      col.names = c("index","date_time","Tm","RH","dew_point"),...)
   header <- readLines(f_path,n = 2)
   header[1] <- gsub("(\xb0C)","",header[1],useBytes = TRUE)
   if(verbose == TRUE){
      cat("\nlogger name: ", strsplit(header[1], split = ",")[[1]][1])
      cat("\nlogger ID: ", strsplit(header[[2]], split = ",")[[1]][6])
      cat("\nstart: ", min(dat$date_time))
      cat("\nend: ", max(dat$date_time),"\n")
   }
   dat$name <- strsplit(header[1], split = ",")[[1]][1]
   dat$serial_n <- strsplit(header[[2]], split = ",")[[1]][6]
   if(length(lon) >1 | length(lat) >1) stop("lon and lat must be a single value")
   dat$lon <- lon
   dat$lat <- lat

   return(dat[,c("name","serial_n","lon","lat","date_time","Tm","RH","dew_point")])
}

lggrs <- list.files("data/weather_loggers",pattern = ".txt", full.names = TRUE)

# read_logger(lggrs[1],verbose = TRUE)[1:5,]

library(data.table)

fwrite(read_logger(lggrs[1],verbose = TRUE,
                   lat = -27.939128,
                   lon = 153.191274),
       "cache/loggers/23_Tamborine_010.csv")

fwrite(read_logger(lggrs[1],verbose = TRUE,
                   lat = -27.939128,
                   lon = 153.191274),
       "cache/loggers/23_Tamborine_010.csv")
