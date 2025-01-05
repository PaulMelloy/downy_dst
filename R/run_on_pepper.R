message("running latest model on 'pepper'")
library(viticolaR)
library(data.table)
library(epiphytoolR)

# sync latest weather
system2("sh", "/home/paul/get_weather.sh")
