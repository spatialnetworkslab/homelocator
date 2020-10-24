#' Nest dataframe 
#' 
#' Nesting creates a list-column of dataframe
#' @param df A dataframe 
#' @param ... A selection of columns. 
#' 
#' 
#' @importFrom emo ji
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map 
#' 
#' @export
nest_verbose <- function(df, ...){
  
  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  
  message(paste(emo::ji("hammer_and_wrench"), "Start nesting..."))
  start.time <- Sys.time()
  output <- df %>% nest_legacy(!!!var_expr)
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message(paste(emo::ji("white_check_mark"), "Finish nesting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Nesting time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Nesting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}


#' Unnest dataframe
#' 
#' Unnesting makes each element of the list its own row.
#' @param df A dataframe
#' @param ... Specification of columns to unnest. 
#' 
#' @importFrom emo ji
#' @export
unnest_verbose <- function(df, ...){
  
  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  
  message(paste(emo::ji("hammer_and_wrench"), "Start unnesting..."))
  start.time <- Sys.time()
  output <- suppressWarnings(
    df %>% unnest_legacy(!!!var_expr))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message(paste(emo::ji("white_check_mark"), "Finish unnesting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}


#' Unnest within a nested dataframe 
#' 
#' Flatten list-column to regular columns inside a nested dataframe
#' 
#' @param df A dataframe
#' @param ... Specification of columns to unnest 
#' 
#' @importFrom emo ji
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map
#' 
#' @export
unnest_nested <- function(df, ...){
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  
  unnest_with_progress <- function(data){
    pb$tick()$print()
    suppressWarnings(
      data %>% unnest_legacy(!!!var_expr)
    )
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start unnesting..."))
  start.time <- Sys.time()
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~unnest_with_progress(.)))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish unnesting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}



#' Nest within a nested dataframe 
#' 
#' Double nesting creates a list-column of nested dataframe
#' @param df A nested dataframe 
#' @param ... A selection of columns. 
#' 
#' @importFrom emo ji
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map
#' @export
nest_nested <- function(df, ...){
  
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])

  nest_with_progress <- function(data){
    pb$tick()$print()
    suppressWarnings(
      data %>% nest_legacy(!!!var_expr)
      )
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  
  message(paste(emo::ji("hammer_and_wrench"), "Start nesting..."))
  start.time <- Sys.time()
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~nest_with_progress(.)))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish nesting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Nesting time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Nesting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}



#' Unnest within a double-nested dataframe
#' 
#' Unnesting makes each element of the list its own row.
#' @param df A nested dataframe
#' @param ... Specification of columns to unnest. 
#' 
#' @importFrom emo ji
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map
#' 
#' @export 
unnest_double_nested <- function(df, ...){
  
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  unnest_with_progress <- function(data){
    pb$tick()$print()
    suppressWarnings(
      data %>% unnest_legacy(!!!var_expr)
    )
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start unnesting..."))
  start.time <- Sys.time()
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~unnest_with_progress(.))) %>% 
    unnest_legacy()
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish unnesting!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}












