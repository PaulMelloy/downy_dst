library(data.table)
library(epiphytoolR)

# find files with desired weather files
zipfiles <- list.files("/homevol/pmelloy/Weather observations/tgz/",pattern = "_IDQ60910.tgz")

for(i in zipfiles){
# Create weather for Applethorpe
#  Stanthorpe only records 3 hourly
merge_axf_weather(File_compressed = paste0("/homevol/pmelloy/Weather observations/tgz/",i),
                     File_axf = "IDQ60910.94553.axf",
                     File_formatted = "23-23_Applethorpe.csv",
                     base_dir = "/homevol/pmelloy/Weather observations/",
                  verbose = TRUE
   )

}
