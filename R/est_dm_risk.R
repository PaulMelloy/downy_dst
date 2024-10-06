#' Title
#'
#' @param days_to_rain
#' @param rain_days input$forecast_rain
#' @param sporangia_survival spo_survival()
#' @param cohort_survival surviving_cz_time()
#' @param zoospores_present surviving_zoospore()
#'
#' @return
#' @export
#'
#' @examples
est_dm_risk <- function(days_to_rain,
                        rain_days,
                        sporangia_survival,
                        cohort_survival,
                        zoospores_present){

   # init risk matrix
   risk_mat <- expand.grid(daytorain = 1:7,
                           raindays = 1:7)

   # ensure days to rain is numeric
   days_to_rain <- ifelse(days_to_rain== "> 7",7,
                       as.numeric(days_to_rain))

   # calculate a cumulative density to determine how likely sporangia will dry out
   #  under this locations climate
   dry_out_factor <-
      1 - ecdf(sporangia_survival)(cohort_survival + (days_to_rain * 24))

   if(zoospores_present == 0){

      # if ther are no surviviing zoospores, estimate the risk based on rainfall
      risk_val <- fcase(quantile(sporangia_survival,0,na.rm = TRUE) > (days_to_rain * 24),
                        4 + (rain_days*0.7),
                        quantile(sporangia_survival,1,na.rm = TRUE) > (days_to_rain * 24),
                        (rain_days*0.7),
                        default = 0)
   }else{
      risk_val <- surviving_zoospore() * rain_days * dry_out_factor}

return(risk_val)


}
