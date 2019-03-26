#' Basic variables 
#' 
#' Add basic variables derive from timestamp 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param timestamp Name of timestamp column. Should be POSIXct
#' 
#' 
derive_timestamp <- function(df, timestamp){
  timestamp_enq <- rlang::enquo(timestamp)
  
  if (!is.data.frame(df)) {
    stop("Error: Dataset is not a dataframe")
  }
  
  if (!is(df %>% pull(!!timestamp_enq), "POSIXct")) {
    stop("Error: Timestamp is not of class POSIXct")
  }
  
  cat("Deriving basic needed variables from timestamp column of the dataset...")
  
  df %>% 
    mutate(year = lubridate::year(!!timestamp_enq),
      month = lubridate::month(!!timestamp_enq),
      day = lubridate::day(!!timestamp_enq), 
      day_of_week = lubridate::wday(!!timestamp_enq),
      hour_of_day = lubridate::hour(!!timestamp_enq), 
      date = as.Date(!!timestamp_enq))
}