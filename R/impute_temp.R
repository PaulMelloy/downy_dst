impute_temp <- function(w, rolling_window = 40){

   # set an index variable
   w[,indx := .I]

   # Get the number of nas
   n_nas <- length(w[is.na(temp), indx])

   if(n_nas == 0){
      message("No NA temperatures")
      return(w)}

   # Check how many NAs remain
   dif <- n_nas

   while(dif > 0) {
      #get nas
      n_nas <- length(w[is.na(temp), indx])
      w[, var_imp := round(
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
      w[is.na(temp), temp := var_imp]

      # Record how many NAs remain
      dif <- n_nas - length(w[is.na(temp), indx])

   }
   return(w)
}
