#' Basic variables 
#' 
#' Add basic variables derive from timestamp 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param timestamp Name of timestamp column. Should be POSIXct
#' 
#' 
# derive_timestamp <- function(df, timestamp){
#   timestamp_enq <- rlang::enquo(timestamp)
#   
#   if (!is.data.frame(df)) {
#     stop("Error: Dataset is not a dataframe")
#   }
#   
#   if (!is(df %>% pull(!!timestamp_enq), "POSIXct")) {
#     stop("Error: Timestamp is not of class POSIXct")
#   }
#   
#   message(paste(emo::ji("hammer_and_wrench"), "Deriving new variables from timestamp..."))
#   output <- df %>% 
#     mutate(year = lubridate::year(!!timestamp_enq),
#            month = lubridate::month(!!timestamp_enq),
#            day = lubridate::day(!!timestamp_enq), 
#            wday = lubridate::wday(!!timestamp_enq), #day of week
#            hour = lubridate::hour(!!timestamp_enq), #hour of day
#            date = as.Date(!!timestamp_enq)) %>% 
#     nest()
#   
#   message(paste(emo::ji("white_check_mark"), "New added variables: year, month, day, wday, hour, date."))
#   output
# }


#' Derive new variables 
#' 
#' Add basic needed variables derived from timestamp 
#' @param df A nested dataframe 
derive_timestamp <- function(df, timestamp){
  
  if(!is.list(df[,grepl("data", names(df))]))
    stop("Error: Dataset is not nested!")
  
  nested_data <- names(df[,grepl("data", names(df))])
  timestamp_enq <- rlang::enquo(timestamp)
  
  
  if(!is(df[[nested_data]][[1]] %>% pull(!!timestamp_enq), "POSIXct")){
    stop("Error: Timestamp is not of class POSIXct")
  }
  
  derive_column <- . %>% 
    mutate(year = lubridate::year(!!timestamp_enq),
           month = lubridate::month(!!timestamp_enq),
           day = lubridate::day(!!timestamp_enq), 
           wday = lubridate::wday(!!timestamp_enq), #day of week
           hour = lubridate::hour(!!timestamp_enq), #hour of day
           date = as.Date(!!timestamp_enq))
  
  message(paste(emo::ji("hammer_and_wrench"), "Deriving new variables from timestamp to each user..."))
  
  output <- df %>%
    mutate(!!nested_data := purrr::map(df[[nested_data]], derive_column))
  message(paste(emo::ji("white_check_mark"), "New added variables to each user: year, month, day, wday, hour, date."))
  output
}













