#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
add_col <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  df %>% 
    mutate(!!!adds_exp_enq)
}

#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
add_col_in_nest <- function(df, ...){
  
  adds_exp_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]
  
  
  add_with_progress <- function(data){
    pb$tick()$print()
    add_column <- data %>% 
      mutate(!!!adds_exp_enq)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Creating variable..."))
  
  output <- df %>%
    mutate(!!nested_data := purrr::map(df[[nested_data]], ~add_with_progress(.))) 
  
  ori_cols <- df[[nested_data]][[1]] %>% names()
  new_cols <- output[[nested_data]][[1]] %>% names()
  added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
  message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
  output
}

#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param var Variable to be calculated 
add_var_pct <- function(df, var){
  var_expr <- enquo(var)
  nested_data <- names(df[,grepl("data", names(df))])
  user_data <- df[[nested_data]]
  
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
  
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Creating variable..."))
  output <- df %>% 
    dplyr::bind_cols(do.call(dplyr::bind_rows, purrr::map(df[[nested_data]], ~add_with_progress(.))))
  
  ori_cols <- names(df)
  new_cols <- names(output)
  added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
  message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
  output
}
#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param group_var The variable to be grouped 
#' @param mutate_vars The variables you want to add to the dataframe

add_groupCol <- function(df, group_vars, mutate_vars){
  
  stopifnot(
    is.list(group_vars),
    is.list(mutate_vars)
  )
  
  df %>%
    group_by(!!!group_vars) %>%
    mutate(!!!mutate_vars) %>%
    ungroup() %>% 
    unnest() 
}






