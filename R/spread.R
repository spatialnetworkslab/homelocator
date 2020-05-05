#' spread variable 
#' 
#' Spread variables to tidy dataframe
#' @param df A dataframe 
#' @param key_var Key variable to be spreaded 
#' @param value_var Value variable to be spreaded
#' 
#' 
#' This was originally called spread2. Qingqing to check why that was?
#' 
spread_nested <- function(df, key_var, value_var){
  
  key_var_enq <- enquo(key_var)
  value_var_enq <- enquo(value_var)
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]

  ori_cols <- names(df[[nested_data]][[1]])
  
  spread_with_progress <- function(data){
    pb$tick()$print()
    spread_column <- data %>%
      spread(key = {{key_var_enq}}, value = {{value_var_enq}}) %>%
      replace(., is.na(.), 0)
  }

  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Spread variable:", quo_name(key_var_enq)))

  
  output <- df %>%
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~spread_with_progress(.)))
  output
}


#' spread variable 
#' 
#' Spread variables to tidy dataframe
#' @param df A nested dataframe 
#' @param full_col Full columns in nested data
#' 
#' 
#' 
mutate_nested_if_missing <- function(df, full_col){
  
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]
  

  fill_with_progress <- function(data){
    pb$tick()$print()
    missed_col <- dplyr::setdiff(full_col, names(data))

    if(purrr::is_empty(missed_col)){
      adds <- data
    } else{
      missed_col_enq <- enquo(missed_col)
      adds <- data %>%
        mutate({{missed_col}} := 0)
    }
  }
  pb <- dplyr::progress_estimated(length(user_data))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Fill missing variables..."))

  output <- df %>%
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~fill_with_progress(.)))
  output
}













