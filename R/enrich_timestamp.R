#' Derive new variables 
#' 
#' Add basic needed variables derived from timestamp 
#' @param df A nested dataframe 
#' @param timestamp Name of timestamp column. Should be POSIXct
enrich_timestamp <- function(df, timestamp, tz = "Asia/Singapore"){
  if(!is.list(df[,grepl("data", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }
  
  nested_data <- names(df[,grepl("data", names(df))])
  timestamp_enq <- rlang::sym(timestamp)
  user_data <- df[[nested_data]]
  
  if(!is(df[[nested_data]][[1]] %>% pull({{timestamp_enq}}), "POSIXct")){
    stop(paste(emo::ji("bomb"), "Error: Timestamp is not of class POSIXct!"))
  }
  
  #define reading function which includes the progress bar
  enrich_with_progress <- function(data){
    pb$tick()$print()
    enrich_cols <- data %>%
      mutate(year = lubridate::year({{timestamp_enq}}),
             month = lubridate::month({{timestamp_enq}}),
             day = lubridate::day({{timestamp_enq}}),
             wday = lubridate::wday({{timestamp_enq}}), #day of week
             hour = lubridate::hour({{timestamp_enq}}), #hour of day
             ymd = as.Date({{timestamp_enq}}, tz = tz))
  }

  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Enrich variables from timestamp for each user..."))
  output <- df %>%
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~enrich_with_progress(.)))
  
  message(paste("\n", emo::ji("white_check_mark"), "New added variables: year, month, day, wday, hour, ymd."))
  output
}











