#' Impute missing Bureau of Meteorology weather data
#'
#' @details
#' Requires sub-hourly data which is summarized to hourly and then missing data
#' imputed.
#'
#'
#' @param path character, file path to csv file with bom weather data.
#' @param variables character vector, including the weather variables to impute
#' @param lon numeric, longitude of the weather station
#' @param lat numeric, latitude of the weather station
#' @param rolling_window integer, number of days to consider when imputing the
#'  rolling_window. Fewer stations result imputed data to close to neighbouring
#'  days. Too many stations result in the same data every time
#' @param min_wd_sd numeric, minimum wind direction standard deviation. Defaults
#' to 50 degrees
#' @param rainNA numeric, what value to fill NA rain observations.
#'
#' @return data.table of weather data
imp_bomstation_data <- function(path,
                                variables = c("temp","rh","rain"),
                                lon,
                                lat,
                                rolling_window = 60,
                                min_wd_sd = 50,
                                rainNA = NA){

   # read in data
   wdata <- fread(path)

   if(length(unique(wdata$name))> 1) stop("Two weather station names exist in weather
                                         file. Please remove data from one of the stations")

   message("\nProcessing ",unique(wdata$name))

   if(missing(lon)){
      lon <- round(mean(wdata$lon),digits = 4)
      wdata$lon <- lon
   }else{
      wdata$lon <- lon
   }
   if(missing(lat)){
      lat <- round(mean(wdata$lat),digits = 4)
      wdata$lat <- lat
   }else{
      wdata$lat <- lat
      }


   wdata[,aifstime_utc := as.POSIXct(as.character(aifstime_utc),
                                     format = "%Y%m%d%H%M%S",
                                     tz = "UTC")]

   wdata <- wdata[order(aifstime_utc)]

   # Check for error entries
   wdata[rain_ten < 0, rain_ten := 0]

   tm_out <- which(wdata$air_temp < -30 |
                      wdata$air_temp > 60)
   wdata[tm_out,air_temp := NA_real_]
   wdata[tm_out, air_temp := frollmean(air_temp,
                                       n = 5,
                                       align = "center",
                                       na.rm = TRUE)]

   rh_out <- which(wdata$rel_hum < 0 |
                      wdata$rel_hum > 100)
   wdata[rh_out,rel_hum := NA_real_]
   wdata[rh_out, rel_hum := frollmean(rel_hum,
                                      n = 5,
                                      align = "center",
                                      na.rm = TRUE)]

   ws_out <- which(wdata$wind_spd_kmh < 0 |
                      wdata$wind_spd_kmh > 150)
   wdata[ws_out, wind_spd_kmh := NA_real_]
   wdata[ws_out, wind_spd_kmh := frollmean(wind_spd_kmh,
                                           n = 5,
                                           align = "center",
                                           na.rm = TRUE)]

   wd_out <- which(wdata$wind_dir_deg < 0 |
                      wdata$wind_dir_deg > 360)
   wdata[wd_out, wind_dir_deg := NA_real_]
   circle_mean <- function(x){
      as.numeric(circular::mean.circular(
         circular::circular(x,
                            units = "degrees",
                            modulo = "2pi"),
         na.rm = TRUE))}
   wdata[wd_out, wind_dir_deg := frollapply(wind_dir_deg,
                                            n = 3,
                                            FUN = circle_mean,
                                            align = "center")]


   # create standard deviation of wind speed
   # given the wikipedia description between wind gusts and average wind speed
   # we will use 1 sd as half the difference between wind speed and wind gusts
   #wdata[, wsp_sd := (gust_kmh - wind_spd_kmh)/2]
   #wdata[, wdir_sd := min_wd_sd]


   # for information on what the column headers relate to
   #  http://www.bom.gov.au/catalogue/Observations-XML.pdf
   wdata <-
      epiphytoolR::format_weather(
         wdata,
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
         impute_nas = c("temp","rh"),
         Irolling_window = rolling_window,
         data_check = FALSE)

   # # impute temperature and humidity
   # wdata <- epiphytoolR::impute_temp(wdata, rolling_window = rolling_window)
   # wdata <- epiphytoolR::impute_rh(wdata, rolling_window = rolling_window)


   # due to the rolling imputation the first or last data could be be NA.
   # and needs to be removed
   na_data <- which(wdata[, is.na(temp) | is.na(rh)])
   message(length(na_data)," lines with NA temp or rh data")
   if(any(na_data < rolling_window)){
      ex_below <- max(na_data[na_data < rolling_window])+1
      wdata <- wdata[ex_below:nrow(wdata)]}

   na_data <-  nrow(wdata) - which(wdata[, is.na(temp) | is.na(rh)])
   if(any(na_data < rolling_window)){
      ex_above <- nrow(wdata) - (max(na_data[na_data < rolling_window])+1)
      wdata <- wdata[1:ex_above]}

   if(nrow(wdata[is.na(rain)]) >=1) {
      wdata[is.na(rain), rain := rainNA]
   }

   # # create standard deviation of wind speed
   # # given the wikipedia description between wind gusts and average wind speed
   # # we will use 1 sd as half the difference between wind speed and wind gusts
   # wdata[, wsp_sd := (gust_kmh - wind_spd_kmh)/2]
   # wdata[, wdir_sd := min_wd_sd]


   return(wdata)

}
