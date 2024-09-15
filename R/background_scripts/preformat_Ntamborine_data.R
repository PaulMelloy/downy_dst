## code to prepare `DATASET` dataset goes here
library(data.table)
library(epiphytoolR)

if(Sys.info()["nodename"] == "viticolr") {
  # read in latest data
   NT_weather <- fread("~/weather_data/23-24_NTamborine.csv")
} else{
  # read in the raw data
   NT_weather <- data.table::fread(
     system.file("extdata",
                 "weather_north_tamborine.csv",
                 package = "viticolaR"))
}

NT_weather[,lon := 153.1914]
NT_weather[,lat := -27.9396]

# format times
NT_weather[,aifstime_utc := as.POSIXct(as.character(aifstime_utc),
                                       format = "%Y%m%d%H%M%S",
                                       tz = "UTC")]

# for information on what the column headers relate to
#  http://www.bom.gov.au/catalogue/Observations-XML.pdf
suppressWarnings(
NT_weather <-
  epiphytoolR::format_weather(
    NT_weather,
    POSIXct_time = "aifstime_utc",
    time_zone = "UTC",
    temp = "air_temp",
    rain = "rain_ten",
    rh = "rel_hum",
    ws = "wind_spd_kmh",
    wd = "wind_dir_deg",
    station = "name",
    lon = "lon",
    lat = "lat",
    data_check = FALSE))

# set the width of the rolling window, this will impact the smoothing of the
#  imputation
rolling_window <- 60
# set the index in the table, this is needed for the rolling apply function
NT_weather[,indx := .I]

## check how many NAs are in the temperature column
# NT_weather[is.na(temp),indx]

# Rolling apply using the impute_fill function on temperature variable
NT_weather[, tm_imp := round(data.table::frollapply(
  indx,
  n = rolling_window,
  fill = NA_real_,
  FUN = epiphytoolR::impute_fill,
  FUN_n = rolling_window,
  times = times,
  var = temp,
  align = "center"
),3)]


# # # visualise the fit of the fill
# plot(NT_weather$temp[1050:1600], type = "l")
# lines(NT_weather$tm_imp[1050:1600], type = "l", col = "blue")
# abline(v = seq(0,550, by = 24))

# set the NAs in temperature with the estimated temperature
NT_weather[is.na(temp), temp:= tm_imp]

# # Check how many NAs remain
# NT_weather[is.na(temp),indx]

# We could widen the rolling_window, so we only need to impute once or we can
#  run the function twice.
# we can run the impute_fill function over the same data to fill in the rest
dif <- 20
while(dif > 0) {
  #get nas
  na_s <- length(NT_weather[is.na(temp), indx])
  NT_weather[, tm_imp := round(
    data.table::frollapply(
      indx,
      n = rolling_window,
      fill = NA_real_,
      FUN = epiphytoolR::impute_fill,
      FUN_n = rolling_window,
      times = times,
      var = temp,
      align = "center"
    ),
    3
  )]

  # set the NAs in temperature with the estimated temperature
  NT_weather[is.na(temp), temp := tm_imp]


  # Check how many NAs remain
  dif <- na_s - length(NT_weather[is.na(temp), indx])
  # cat("Dif remaining: ", dif,"\n")
}
# # visualise the fit of the fill
# plot(NT_weather$temp[2200:2243], type = "l")
# lines(NT_weather$tm_imp[2200:2243], type = "l", col = "blue")
# abline(v = seq(0,43, by = 24))


# Impute Relative humidity _____________________
dif <- length(NT_weather[is.na(rh), indx])
while(dif > 0) {
  #get nas
  na_s <- length(NT_weather[is.na(rh), indx])
  NT_weather[, rh_imp := round(
    data.table::frollapply(
      indx,
      n = rolling_window,
      fill = NA_real_,
      FUN = epiphytoolR::impute_fill,
      FUN_n = rolling_window,
      times = times,
      var = rh,
      align = "center"
    ),
    3
  )]
  # set the NAs in temperature with the estimated temperature
  NT_weather[is.na(rh), rh := rh_imp]
  # Check how many NAs remain
  dif <- na_s - length(NT_weather[is.na(rh), indx])
  ## Report
  # cat("Dif remaining: ", dif,"\n")
}



# # check how many NA entries remain and trim them
# NT_weather[, .(temp_nas = sum(is.na(temp)),
#       rh_nas = sum(is.na(rh)))]
# dim(NT_weather)
# Remove lines with NAs
NT_weather <- NT_weather[1:(which(is.na(temp))[1]-1)]
# dim(NT_weather)

# fill rain NAs with 0
NT_weather[is.na(rain), rain := 0]

# remove imp columns
NT_weather[,c("tm_imp", "rh_imp"):= list(NULL,NULL)]

DMod <- viticolaR::estimate_DM_PI(NT_weather)

save(NT_weather, DMod, file = "~/downy_dst/DM_dst_data.rda")
