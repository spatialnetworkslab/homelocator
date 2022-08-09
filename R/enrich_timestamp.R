#' Create new variables dervied from timestamp
#' 
#' Create new variables from existing timestamp, which are often used/needed as intermediate variables in home location algorithms. 
#' @param df A nested dataframe 
#' @param timestamp Name of column that holds specific timestamp for each data point and it should be POSIXct
#' 
#' @importFrom emo ji
#' @importFrom rlang sym
#' @importFrom lubridate year 
#' @importFrom lubridate month 
#' @importFrom lubridate day 
#' @importFrom lubridate wday 
#' @importFrom lubridate hour 
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map 
#' 
#' @export
enrich_timestamp <- function(df, timestamp = "created_at"){
  if(!is.list(df[ , grepl("^data$", names(df)), with=F])){
    stop(paste(emo::ji("bomb"), "Input dataset is not nested!"))
  }
  
  colname_nested_data <- names(df[ , grepl("^data$", names(df)), with=F])
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
             ymd = format({{timestamp}}, "%Y-%m-%d"))
  }

  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Enriching variables from timestamp..."))
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~enrich_with_progress(.)))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish enriching! New added variables: year, month, day, wday, hour, ymd."))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Enriching time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Enriching time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}












