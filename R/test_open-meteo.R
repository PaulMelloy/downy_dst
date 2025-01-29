library(openmeteo)
library(data.table)
library(epiphytoolR)
library(ggplot2)

# read in Mt tamborine data
tam <- fread("data/23-24_NTamborine.csv")
tam[, datetime := as.POSIXct(as.character(aifstime_utc), format = "%Y%m%d%H%M%S")]
tam[, lon := mean(lon)]
tam[, lat := mean(lat)]



tamf <-
   format_weather(tam,
               POSIXct_time = "datetime",
               time_zone = "UTC",
               temp = "air_temp",
               rh = "rel_hum",
               ws = "wind_spd_kmh",
               wd = "wind_dir_deg",
               rain = "rain_ten",
               station = "name",
               lon = "lon",
               lat = "lat",
               data_check = c("temp","rain","rh"),
               impute_nas = FALSE
)


if(isFALSE(file.exists("data/23-25_openmet_tamborine.csv"))){
tamo <- weather_history(c(-27.9396, 153.1914),
                        "2023-06-01",
                        "2025-01-20",
                        hourly = c("temperature_2m",
                                   "relative_humidity_2m",
                                   "cloud_cover",
                                   "wind_speed_10m",
                                   "wind_direction_10m",
                                   "wind_gusts_10m",
                                   "shortwave_radiation",
                                   "direct_radiation",
                                   "vapour_pressure_deficit",
                                   "et0_fao_evapotranspiration",
                                   "rain",
                                   "soil_temperature_0_to_7cm",
                                   "soil_moisture_0_to_7cm"
                                   ),
                        timezone = "UTC")

setDT(tamo)
fwrite(tamo, "data/23-25_openmet_tamborine.csv")}else{
   tamo <-fread("data/23-25_openmet_tamborine.csv")
}

tamf[, datetime := times + (60*60*10)]

tamof <- tamf[tamo, on = "datetime"]

# tamof[times > as.POSIXct("2023-12-01") & times < as.POSIXct("2023-12-31")] |>
#    ggplot(aes(x = datetime, y = hourly_temperature_2m )) +
#    geom_line() +
#    geom_line(aes(y = temp), colour = "blue")
#
