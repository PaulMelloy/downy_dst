library(data.table)
library(epiphytoolR)

# find files with desired weather files
zipfiles <- list.files("/homevol/pmelloy/Weather observations/tgz/",pattern = "_IDW60910.tgz")

for(i in zipfiles){
# Create weather for Applethorpe
#  Stanthorpe only records 3 hourly
merge_axf_weather(File_compressed = paste0("/homevol/pmelloy/Weather observations/tgz/",i),
                     File_axf = "IDW60910.95641.axf",
                     File_formatted = "23-24_WitchcliffeWest.csv",
                     base_dir = "/homevol/pmelloy/Weather observations/",
                  verbose = TRUE
   )

}

# Do for Mildura airport
# find files with desired weather files
zipfiles <- list.files("/homevol/pmelloy/Weather observations/tgz/",pattern = "_IDS60910.tgz")

for(i in zipfiles){
   # Create weather for Mildura
   #  Stanthorpe only records 3 hourly
   merge_axf_weather(File_compressed = paste0("/homevol/pmelloy/Weather observations/tgz/",i),
                     File_axf = "IDS60910.95687.axf",
                     File_formatted = "23-23_RenmarkAP.csv",
                     base_dir = "/homevol/pmelloy/Weather observations/",
                     verbose = TRUE
   )

}


