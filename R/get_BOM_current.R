get_BOM_current <- function(){

   library(RCurl)
   library(xml2)
   library(data.table)
   library(tidyr)

   URL <- "ftp://ftp.bom.gov.au/anon/gen/fwo/"
   # filenames <- getURL(URL,ftp.use.epsv = FALSE, dirlistonly = TRUE)
   # filenames <- unlist(strsplit(filenames, "\r\n"))
   # filenames

   dl_floc <- paste0(getwd(),"/Weather observations/xml/",format(Sys.time(), format = "%y%m%d_%H%M"),"_IDQ60920.xml")

   download.file(url = paste0(URL,"IDQ60920.xml"),
                 destfile = dl_floc)

   bom_xml <-read_xml(dl_floc)

   xml_children(bom_xml)
   xml_contents(bom_xml)

   # Observataions
   lvl1_obs <-
      xml_child(bom_xml, search = 2)

   xml_children(lvl1_obs)
   xml_length(lvl1_obs)
   xml_attrs(lvl1_obs)

   ## Station
   lvl2_station <-
      xml_child(# Station
         lvl1_obs
         )

   xml_children(lvl2_station)
   xml_attrs(lvl2_station)

   ### Period
   lvl3_period <-
      xml_child(lvl2_station)

   xml_children(lvl3_period)
   xml_length(lvl3_period)
   xml_attrs(lvl3_period)

   #### level
   lvl4_level <-
      xml_child(lvl3_period)

   xml_children(lvl4_level)
   xml_length(lvl4_level)
   xml_attrs(lvl4_level)

   ##### element
   lvl4_level
   xml_child(lvl4_level,17) # rain recording duration in local and UTC
   xml_child(lvl4_level,18) # time range when max air temperature was recorded
   xml_child(lvl4_level,19) # time range when min air temperature was recorded
   xml_child(lvl4_level,20) # time range when max wind gust speed calculated (knots)
   xml_child(lvl4_level,21) # time range when max wind gust speed calculated (kph)
   xml_child(lvl4_level,22) # time range when max wind gust direction was determined



   xml_children(lvl4_level)
   xml_length(lvl4_level)
   xml_attrs(lvl4_level)



   xml_parents(bom_xml)
   xml_siblings(bom_xml)
   xml_length(bom_xml)

   xml_ns(bom_xml)

   bom_list <- as_list(xml_child(bom_xml, 2))

   bom_tb <-
      as_tibble(bom_list) |>
      unnest_wider()
   unnest_wider(bom_list)


   # get all the <record>s
   recs <- xml_find_all(bom_xml, "//station")

   xml_attr(recs,attr = "wmo-id")
   rbind(xml_attrs(recs[1]))


   station_list <-
      lapply(recs,function(x){
         as.data.frame(
            as.list(
               xml_attrs(x)
            ))
      })

   station_dt <-
      rbindlist(station_list)




   # extract and clean all the columns
   vals <- trimws(xml_text(recs))

   # extract and clean (if needed) the area names
   labs <- trimws(xml_attr(recs, "label"))

   xml_find_all(bom_xml,"//station")





}
