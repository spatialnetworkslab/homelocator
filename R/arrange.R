#' Arrange rows by variables within a nested dataframe
#' 
#' Arrange rows by variables within a nested dataframe
#' @param df A nested dataframe 
#' @param ... Comma separated list of unquoted variable names
#' 
#' @importFrom dplyr progress_estimated
#' @importFrom emo ji
#' @importFrom purrr map
#' @export
arrange_nested <- function(df, ...){
  var_expr <- enquos(...)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  arrange_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      arrange(!!!var_expr)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
 
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Start sorting..."))
  output <- df %>%
    dplyr::mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~arrange_with_progress(.))) 
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)

  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish sorting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Sorting time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Sorting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}


#' Arrange rows by variables within a double-nested dataframe
#' 
#' Arrange rows by variables within a double-nested dataframe
#' @param df A nested dataframe 
#' @param nest_cols Name of columns to nest in existing list-column
#' @param ... Comma separated list of unquoted variable names
#' 
#' @importFrom emo ji
#' @importFrom purrr map
#' 
#' @export
arrange_double_nested <- function(df, nest_cols, ...){
  if(nrow(df) == 0){
    stop(paste(emo::ji("bomb"), "No user left, tune your threshold and try again."))
  }
  
  stopifnot(
    is.list(df[ , grepl("^data$", names(df))])
  )
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  arrange_column <- . %>% arrange(!!!var_expr)
  
  arrange_columns <- . %>% 
    dplyr::mutate(data = purrr::map(data, arrange_column)) 
  
  # double nest 
  df[[colname_nested_data]] <- purrr::map(df[[colname_nested_data]], ~.x %>% nest(data = nest_cols))
  
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Start sorting..."))
  output <- df %>% 
    dplyr::mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], arrange_columns))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)

  message(paste(emo::ji("white_check_mark"), "Finish sorting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Sorting time:", time.taken, "mins"))
  } else{
    message(paste(emo::ji("hourglass"), "Sorting time:", time.taken, "secs"))
  }
  
  message("\n")
  return(output)
}