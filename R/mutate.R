#' Mutate columns inside of nested data 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param ... Variables to be added
#'

mutate_nested <- function(df, ...){
  
  adds_exp_enq <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[,grepl("data$", names(df))])

  add_with_progress <- function(data){
    pb$tick()$print()
    add_column <- data %>% 
      mutate(!!!adds_exp_enq)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Creating variable..."))
  
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~add_with_progress(.))) 
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original) %>% paste(., collapse = ", ")
 
  message("\n")
  message(paste(emo::ji("white_check_mark"), "New added variables:", colnames_added))
  output
}

#' Calculate relative frequency or proportion per category in variable
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param var Variable to be calculated 
#' 
#' 

prop_factor_nested <- function(df, var){
  var_expr <- enquo(var)
  colname_nested_data <- names(df[,grepl("data", names(df))])

  add_with_progress <- function(data){
    pb$tick()$print()
    add_pct <- data %>% 
      pull(!!var_expr) %>% 
      table() %>% 
      prop.table() %>% 
      as_tibble() %>% 
      setNames(c("var", "pct")) %>% 
      spread(var, pct)
  }
  
  pb <- dplyr::progress_estimated(nrow(df))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Creating variable..."))
  output <- df %>% 
    dplyr::bind_cols(do.call(dplyr::bind_rows, purrr::map(df[[colname_nested_data]], ~add_with_progress(.))))
  
  colnames_original <- names(df)
  colnames_new <- names(output)
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original) %>% paste(., collapse = ", ")
  message("\n")
  message(paste(emo::ji("white_check_mark"), "New added variables:", colnames_added))
  output
}


#' Mutate inside nested data, but doing so per group 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param group_var The variable to be grouped 
#' @param mutate_vars The variables you want to add to the dataframe
#' 
mutate_nested_by_group <- function(df, group_vars, mutate_vars){
  
  stopifnot(
    is.list(group_vars),
    is.list(mutate_vars)
  )
  
  colname_nested_data <- names(df[,grepl("data", names(df))])

  add_bygrp_with_progress <- function(data){
    pb$tick()$print()
    add_bygrp <- data %>% 
      group_by(!!!group_vars) %>%
      mutate(!!!mutate_vars) %>%
      ungroup() 
  }
  pb <- dplyr::progress_estimated(nrow(df))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Creating variable..."))
  
  output <- df %>% 
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~add_bygrp_with_progress(.))) 
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original) %>% paste(., collapse = ", ")
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "New added variables:", colnames_added))
  output
}

