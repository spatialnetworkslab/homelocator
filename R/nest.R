#' Nest dataframe 
#' 
#' Nesting creates a list-column of dataframe
#' @param df A dataframe 
#' @param ... A selection of columns. 
#' 
#' 
nest_verbose <- function(df, ...){
  
  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  
  message(paste(emo::ji("hammer_and_wrench"), "Start nesting..."))
  start.time <- Sys.time()
  output <- df %>% nest_legacy(!!!var_expr)
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  message(paste(emo::ji("white_check_mark"), "Finish nesting!"))
  message(paste(emo::ji("hourglass"), "Nesting time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}


#' Unnest a list column
#' 
#' Unnesting makes each element of the list its own row.
#' @param df A dataframe
#' @param ... Specification of columns to unnest. 
#' 
#' 
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
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  message(paste(emo::ji("white_check_mark"), "Finish unnesting!"))
  message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}


#' Unnest in nested dataframe 
#' 
#' Flatten list-column to regular columns inside a nested dataframe
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
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish unnesting!"))
  message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}



#' Nest nested dataframe 
#' 
#' Double nesting creates a list-column of nested dataframe
#' @param df A nested dataframe 
#' @param ... A selection of columns. 
#' 
#' 
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
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish nesting!"))
  message(paste(emo::ji("hourglass"), "Nesting time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}



#' Nunest dataframe
#' 
#' Unnesting makes each element of the list its own row.
#' @param df A nested dataframe
#' @param ... Specification of columns to unnest. 
#' 
#' 
#' 
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
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish unnesting!"))
  message(paste(emo::ji("hourglass"), "Unnesting time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}












