#' Return rows with matching conditions 
#' 
#' Filter finds rows where conditions are true. 
#' @param df A dataframe 
#' @param user Name of column that holds unique identifier for each user
#' @param ... Logical predicates defined in terms of the variables in df. Only rows match conditions are kept.
#' 
#' 
filter_verbose <- function(df, user = "u_id", ...){
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  var_expr <- enquos(...)
  
  n_original_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_original_users, "users at this moment."))
  message(paste(emo::ji("hammer_and_wrench"), "Start filtering users..."))
  
  output <- df %>% 
    filter(!!!var_expr)
  
  n_new_users <- output %>% pull({{user}}) %>% n_distinct()
  n_removed_users <- n_original_users - n_new_users
  message(paste(emo::ji("white_check_mark"), "Filter", n_removed_users, "users!"))
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_new_users, "users left."))
  
  return(output)
}



#' Return rows with matching condition in list-column 
#' 
#'   
#' Filter finds rows where conditions are true in list-column
#' @param df A nested dataframe 
#' @param user Name of column that holds unique identifier for each user
#' @param ... Logical predicates defined in terms of the variables in df. Only rows match conditions are kept.
#' 
#' 
filter_nested <- function(df,  user = "u_id", ...){
  
  if(!is.list(df[ , grepl("^data$", names(df))])){
    stop(paste(emo::ji("bomb"), "Dataset is not nested!"))
  }
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  var_expr <- enquos(...)
  user <- rlang::sym(user) 
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  #filter
  filter_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      filter(!!!var_expr)
  }
  
  n_original_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_original_users, "users at this moment."))
  message(paste(emo::ji("hammer_and_wrench"), "Start filtering user...")) 
  
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  output <- df %>% 
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~filter_with_progress(.))) 
  
  output_data <- output[[colname_nested_data]]
  
  #check empty tibble 
  output <- output %>% 
    filter(!(purrr::map_lgl(output_data, plyr::empty)))
    
  n_new_users <- output %>% pull({{user}}) %>% dplyr::n_distinct()
  n_removed_users <- n_original_users - n_new_users
  message(paste(emo::ji("white_check_mark"), "Filter", n_removed_users, "users!"))
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_new_users, "users left!"))
 
  return(output)
}









