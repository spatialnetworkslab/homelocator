#' Spread a key-value pair across multiple columns
#' 
#' Spread a key-value pair across multiple columns in nested dataframe 
#' @param df A nested dataframe 
#' @param var_key Column name or position
#' @param var_value Column name or position
#' 
#' 
#' @importFrom rlang sym
#' @importFrom dplyr progress_estimated
#' @importFrom dplyr setdiff
#' @importFrom purrr map
#' @importFrom emo ji
#' @export
spread_nested <- function(df, key_var, value_var){
  
  key_var_expr <-  rlang::sym(key_var)
  value_var_expr <-  rlang::sym(value_var)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])

  spread_with_progress <- function(data){
    pb$tick()$print()
    data %>%
      spread(key = {{key_var_expr}}, value = {{value_var_expr}}) %>%
      replace(., is.na(.), 0)
  }

  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start spreading", key_var, "variable..."))
  start.time <- Sys.time()
  output <- df %>%
    dplyr::mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~spread_with_progress(.)))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  colnames_original <- names(df[[colname_nested_data]][[1]])
  colnames_new <- names(output[[colname_nested_data]][[1]])
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original) 
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish spreading! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  
  if(time.taken > 60){
    time.taken <-  round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Spreading time:", time.taken, "mins"))
  } else{
    message(paste(emo::ji("hourglass"), "Spreading time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}










