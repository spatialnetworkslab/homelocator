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
  
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Start adding..."))
  output <- df %>% mutate(!!!var_expr)
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish adding!"))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  message(paste(emo::ji("hourglass"), "Adding time:", time.taken, "mins"))
  
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
  
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])

  add_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      mutate(!!!var_expr)
  }
  
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Start adding variable(s)..."))
  
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~add_with_progress(.))) 
  
  end.time <- Sys.time()
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish adding!"))
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)      
  message(paste(emo::ji("white_check_mark"), "There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  message(paste(emo::ji("hourglass"), "Adding time:", time.taken, "mins"))
  
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
  
  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Start calculating proportion..."))
  
  output <- df %>% 
    dplyr::bind_cols(do.call(dplyr::bind_rows, purrr::map(df[[colname_nested_data]], ~add_with_progress(.)))) %>% 
    replace(., is.na(.), 0)
  
  end.time <- Sys.time()
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish calculating!"))
  
  colnames_original <- names(df)
  colnames_new <- names(output)
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)
  message(paste(emo::ji("white_check_mark"), "There are", length(colnames_added), "new calculated variables:", paste(colnames_added, collapse = ", ")))
  
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  message(paste(emo::ji("hourglass"), "Calculating time:", time.taken, "mins"))
  
  return(output)
}


#' Mutate inside nested data, but doing so per group 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param group_var The variable to be grouped 
#' @param mutate_vars The variables you want to add to the dataframe
#' 
# mutate_nested_by_group <- function(df, group_vars, mutate_vars){
#   
#   stopifnot(
#     is.list(group_vars),
#     is.list(mutate_vars)
#   )
#   
#   colname_nested_data <- names(df[,grepl("data", names(df))])
# 
#   add_bygrp_with_progress <- function(data){
#     pb$tick()$print()
#     add_bygrp <- data %>% 
#       group_by(!!!group_vars) %>%
#       mutate(!!!mutate_vars) %>%
#       ungroup() 
#   }
#   pb <- dplyr::progress_estimated(nrow(df))
#   message("\n")
#   message(paste(emo::ji("hammer_and_wrench"), "Creating variable..."))
#   
#   output <- df %>% 
#     mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~add_bygrp_with_progress(.))) 
#   
#   colnames_original <- df[[colname_nested_data]][[1]] %>% names()
#   colnames_new <- output[[colname_nested_data]][[1]] %>% names()
#   colnames_added <- dplyr::setdiff(colnames_new, colnames_original) %>% paste(., collapse = ", ")
#   
#   message("\n")
#   message(paste(emo::ji("white_check_mark"), "New added variables:", colnames_added))
#   output
# }


