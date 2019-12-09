#' spread variable 
#' 
#' Spread variables to tidy dataframe
#' @param df A dataframe 
#' @param key_var Key variable to be spreaded 
#' @param value_var Value variable to be spreaded
spread2_in_nest <- function(df, key_var, value_var){
  
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
  message(paste(emo::ji("hammer_and_wrench"), "Spread variable:", quo_name(key_var)))

  
  output <- df %>%
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~spread_with_progress(.)))
  output
  # spread_col <- output[[nested_data]][[1]] %>% names()
  # vars <- dplyr::setdiff(spread_col, ori_cols)
  # 
  # fill_with_progress <- function(data){
  #   pb$tick()$print()
  #   missed_col <- dplyr::setdiff(vars, names(data))
  #   
  #   if(purrr::is_empty(missed_col)){
  #     adds <- data
  #   } else{
  #     missed_col_enq <- enquo(missed_col)
  #     adds <- data %>% 
  #       mutate({{missed_col}} := 0)
  #   }
  # }
  # pb <- dplyr::progress_estimated(length(user_data))
  # message("\n")
  # message(paste(emo::ji("hammer_and_wrench"), "Fill missing variables..."))
  # 
  # result <- output %>%
  #   mutate({{nested_data}} := purrr::map(output[[nested_data]], ~fill_with_progress(.))) 
  # result
}