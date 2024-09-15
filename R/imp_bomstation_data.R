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

   if(length(unique(wdata$name))> 1) stop("Two weather station names exist in weather
                                         file. Please remove data from one of the stations")

   # read in data
   wdata <- fread(path)

   wdata$lon <- lon
   wdata$lat <- lat

   wdata[,aifstime_utc := as.POSIXct(as.character(aifstime_utc),
                                     format = "%Y%m%d%H%M%S",
                                     tz = "UTC")]


   # create standard deviation of wind speed
   # given the wikipedia description between wind gusts and average wind speed
   # we will use 1 sd as half the difference between wind speed and wind gusts
   wdata[, wsp_sd := (gust_kmh - wind_spd_kmh)/2]
   wdata[, wdir_sd := min_wd_sd]


   # for information on what the column headers relate to
   #  http://www.bom.gov.au/catalogue/Observations-XML.pdf
   suppressWarnings(
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
            wd_sd = "wdir_sd",
            station = "name",
            lon = "lon",
            lat = "lat",
            data_check = FALSE))

   # impute temperature and humidity
   wdata <- epiphytoolR::impute_temp(wdata, rolling_window = rolling_window)
   wdata <- epiphytoolR::impute_rh(wdata, rolling_window = rolling_window)


   # due to the rolling imputation the last data will be NA. Remove this data``
   if(nrow(wdata[is.na(temp) &
                 is.na(rh)]) >=1){
      wdata <- wdata[1:(which(is.na(temp) &
                                 is.na(rh))[1]-1)]}

   if(nrow(wdata[is.na(rain)]) >=1) {
      wdata[is.na(rain), rain := rainNA]
   }

   # create standard deviation of wind speed
   # given the wikipedia description between wind gusts and average wind speed
   # we will use 1 sd as half the difference between wind speed and wind gusts
   wdata[, wsp_sd := (gust_kmh - wind_spd_kmh)/2]
   wdata[, wdir_sd := min_wd_sd]


   return(wdata)

}
