#' Aggregate multiple values to a single value within a nested tibble
#' 
#' Aggregate multiple values to a single value of existing table by name-value paired summary function within a nested tibble
#' @param df A nested dataframe 
#' @param ... Name-value pairs of summary functions.
#' 
#' @importFrom emo ji
#' @importFrom dplyr progress_estimated
#' @importFrom dplyr setdiff
#' @importFrom purrr map
#' 
#' @export
summarise_nested <- function(df, ...){
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Dataset is not nested!"))
  }

  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  summarise_with_progress <- function(data){
    pb$tick()$print()
    data %>% dplyr::summarise(!!!var_expr)
  }
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))

  message(paste(emo::ji("hammer_and_wrench"), "Start summarising values..."))
  start.time <- Sys.time()
  output <- df %>%
    dplyr::mutate(adds = purrr::map(df[[colname_nested_data]], ~summarise_with_progress(.))) %>%
    unnest_legacy(adds)
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  colnames_original <- names(df)
  colnames_new <- names(output)
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original) 
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish summarising! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Summarising time:", time.taken, "mins"))
  } else{
    message(paste(emo::ji("hourglass"), "Summarising time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}


#' Aggregate multiple values to a single value in a double-nested tibble
#' 
#' Create a list-column in existing list-column and aggregate multiple values in created list-column to a single value by name-value paired summary function
#' @param df A nested dataframe 
#' @param nest_cols A selection of columns to nest in existing list-column
#' @param ... Name-value pairs of summary functions
#' 
#' @importFrom emo ji
#' @importFrom dplyr setdiff
#' @importFrom purrr map
#' 
#' @export 
summarise_double_nested <- function(df, nest_cols, ...){
  
  if(nrow(df) == 0){
    stop(paste(emo::ji("bomb"), "No user left, tune your threshold and try again."))
  }
  
  stopifnot(
    is.list(df[ , grepl("^data$", names(df))])
  )
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  cal_column <- . %>% dplyr::summarise(!!!var_expr)
  
  add_column <- . %>% 
      dplyr::mutate(adds = purrr::map(data, cal_column)) %>% 
      unnest_legacy(adds) 
  
  # double nest 
  df[[colname_nested_data]] <- purrr::map(df[[colname_nested_data]], ~.x %>% nest(data = nest_cols))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start summarising values..."))
  start.time <- Sys.time()
  output <- df %>% 
    dplyr::mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], add_column))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_new <- colnames_new[-which(colnames_new == "data")]
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)
  
  message(paste(emo::ji("white_check_mark"), "Finish summarising! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Summarising time:", time.taken, "mins"))
  } else{
    message(paste(emo::ji("hourglass"), "Summarising time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}



