#' Create variables 
#' 
#' Create new variables from existing timestamp
#' @param df A nested dataframe 
#' @param timestamp Name of column that holds specific timestamp for each data point and it should be POSIXct
#' @param tz A character string containing the time zone to convert to and it should be recognized in R. 
#' 
#' 
enrich_timestamp <- function(df, timestamp = "created_at", tz = "Asia/Singapore"){
  
  if(!is.list(df[ , grepl("data", names(df))])){
    stop(paste(emo::ji("bomb"), "Input dataset is not nested!"))
  }
  
  colname_nested_data <- names(df[,grepl("data", names(df))])
  timestamp <- rlang::sym(timestamp)
 
  if(!is(df[[colname_nested_data]][[1]] %>% pull({{timestamp}}), "POSIXct")){
    stop(paste(emo::ji("bomb"), "Timestamp is not POSIXct!"))
  }
  
  #define reading function which includes the progress bar
  enrich_with_progress <- function(data){
    pb$tick()$print()
    data %>%
      mutate(year = lubridate::year({{timestamp}}),
             month = lubridate::month({{timestamp}}),
             day = lubridate::day({{timestamp}}),
             wday = lubridate::wday({{timestamp}}), # day of the week
             hour = lubridate::hour({{timestamp}}), # hour of the day
             ymd = as.Date({{timestamp}}, tz = tz))
  }

  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Enriching variables from timestamp..."))
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~enrich_with_progress(.)))
  message(paste(emo::ji("white_check_mark"), "New added variables: year, month, day, wday, hour, ymd."))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  message(paste(emo::ji("hourglass"), "Enriching time:", time.taken, "mins"))
  
  return(output)
}












