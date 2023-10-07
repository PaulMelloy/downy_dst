library(RCurl)
library(data.table)

# BOM ftp site address
URL <- "ftp://ftp.bom.gov.au/anon/gen/fwo/"

# specify the time of download
dl_time <- format(Sys.time(), format = "%y%m%d_%H%M")

# save the download file locaiton
dl_floc <-
   paste0(getwd(),
          "/Weather observations/tgz/",
          dl_time,
          "_IDQ60910.tgz")

# download the tar zipped file to download location
download.file(url = paste0(URL, "IDQ60910.tgz"),
              destfile = dl_floc)

# create folder to extract compressed file
dir_loc <- paste0("Weather observations/", dl_time, "/")
dir.create(dir_loc)

# uncompress file
untar(tarfile = dl_floc,
      exdir = dir_loc)


# Files <- list.files(dir_loc,pattern = "axf")
#
# for(i in Files){
#    loc1 <- readLines(paste0(dir_loc,i), n =20)[11]
#    cat(i," : ", loc1," \n")
# }

# Read data
dat <-
   fread(paste0(dir_loc, "IDQ60910.99123.axf"),
         skip = 24,
         nrows = 144)

colnames(dat) <-
   gsub(pattern = "\\[80]", replacement = "", colnames(dat))
dat
Tamborine <- fread(file = paste0(dir_loc,"NTamborine.csv"))

Merged <- rbind(Tamborine,dat)

Merged <- Merged[!duplicated(aifstime_utc)]



fwrite(Merged,file = paste0(dir_loc,"NTamborine.csv"))
