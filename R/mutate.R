#' Add new variable 
#' 
#' Add new variable and perserves existing to dataframe 
#' 
#' @param df A dataframe 
#' @param ... Name-value pairs of expressions
#' 
mutate_verbose <- function(df, ...){
  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  
  message(paste(emo::ji("hammer_and_wrench"), "Start adding..."))
  start.time <- Sys.time()
  output <- df %>% mutate(!!!var_expr)
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  colnames_original <- names(df)
  colnames_new <- names(output)
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)   
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish adding! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  message(paste(emo::ji("hourglass"), "Adding time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}

#' Add new variables in list-column
#' 
#' Add new variables and preserves existing from list-column
#' @param df A nested dataframe 
#' @param ... Name-value pairs of expressions
#'

mutate_nested <- function(df, ...){
  
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Dataset is not nested!"))
  }
  
  var_expr <- enquos(...)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])

  add_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      mutate(!!!var_expr)
  }
  
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  
  message(paste(emo::ji("hammer_and_wrench"), "Start adding variable(s)..."))
  start.time <- Sys.time()
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~add_with_progress(.))) 
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)   
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish adding! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  message(paste(emo::ji("hourglass"), "Adding time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}

#' Calculate relative frequency 
#' 
#' Calculate relative frequency per category of the variable and spread categories to columns
#' @param df A nested dataframe 
#' @param var Name of column to calculate 
#' 
#' 
prop_factor_nested <- function(df, ...){
  
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Dataset is not nested!"))
  }
  
  var_expr <- enquos(...)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])

  add_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      dplyr::select(!!!var_expr) %>% 
      rownames_to_column(var = "id") %>% 
      gather(key = "key", value = "value", -id) %>% 
      group_by(key, value) %>% 
      dplyr::summarise(n = n()) %>% 
      group_by(key) %>% 
      mutate(total = sum(n), 
             prop = n/total) %>% 
      ungroup() %>% 
      dplyr::select(value, prop) %>% 
      spread(value, prop)
  }

  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start calculating proportion..."))
  start.time <- Sys.time()
  output <- df %>% 
    dplyr::bind_cols(do.call(dplyr::bind_rows, purrr::map(df[[colname_nested_data]], ~add_with_progress(.)))) %>% 
    replace(., is.na(.), 0)
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  colnames_original <- names(df)
  colnames_new <- names(output)
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)
  
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish calculating! There are", length(colnames_added), "new calculated variables:", paste(colnames_added, collapse = ", ")))
  message(paste(emo::ji("hourglass"), "Calculating time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}


#' Add new variable 
#' 
#' Add new variable in double nested dataset
#' @param df A dataframe 
#' @param nest_cols A selection of columns to nest in existing list-column
#' @param ... Name-value pairs of functions
#' 
mutate_double_nested <- function(df, nest_cols, ...){

  if(nrow(df) == 0){
    stop(paste(emo::ji("bomb"), "No user left, tune your threshold and try again."))
  }
  
  stopifnot(
    is.list(df[ , grepl("^data$", names(df))])
  )
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])

  add_variable <- . %>% mutate(!!!var_expr)
  add_column <- . %>% 
    mutate(data = purrr::map(data, add_variable)) 
  
  
  # double nest 
  df[[colname_nested_data]] <- purrr::map(df[[colname_nested_data]], ~.x %>% nest(data = nest_cols))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start adding values..."))
  start.time <- Sys.time()
  output <- df %>% 
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], add_column))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_new <- colnames_new[-which(colnames_new == "data")]
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)
  
  message(paste(emo::ji("white_check_mark"), "Finish adding! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  message(paste(emo::ji("hourglass"), "Adding time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}














