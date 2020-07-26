#' Return rows with matching conditions 
#' 
#' Filter finds rows where conditions are true. 
#' @param df A dataframe 
#' @param user Name of column that holds unique identifier for each user
#' @param ... Logical predicates defined in terms of the variables in df. Only rows match conditions are kept.
#' 
#' @export
filter_verbose <- function(df, user = "u_id", ...){
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  var_expr <- enquos(...)
  
  n_original_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_original_users, "users at this moment."))
  message(paste(emo::ji("hammer_and_wrench"), "Start filtering users..."))
  start.time <- Sys.time()
  output <- df %>% filter(!!!var_expr)
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  n_new_users <- output %>% pull({{user}}) %>% n_distinct()
  n_removed_users <- n_original_users - n_new_users

  message(paste(emo::ji("white_check_mark"), "Finish filtering! Filterred", n_removed_users, "users!"))
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_new_users, "users left."))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Filtering time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Filtering time:", time.taken, "secs"))
  }
  
  message("\n")
  return(output)
}

#' Return rows with matching condition within nested dataframe 
#' 
#'   
#' Filter finds rows where conditions are true within nested dataframe 
#' @param df A nested dataframe 
#' @param user Name of column that holds unique identifier for each user
#' @param ... Logical predicates defined in terms of the variables in df. Only rows match conditions are kept.
#' 
#' @export
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
  
  start.time <- Sys.time()
  n_original_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_original_users, "users at this moment."))
  message(paste(emo::ji("hammer_and_wrench"), "Start filtering user...")) 
  
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  start.time <- Sys.time()
  output <- df %>% 
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~filter_with_progress(.))) 
  output_data <- output[[colname_nested_data]]
  #check empty tibble 
  output <- output %>% 
    filter(!(purrr::map_lgl(output_data, plyr::empty)))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  n_new_users <- output %>% pull({{user}}) %>% dplyr::n_distinct()
  n_removed_users <- n_original_users - n_new_users
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish Filtering! Filterred", n_removed_users, "users!"))
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_new_users, "users left!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Filtering time:", time.taken, "mins"))
  }else{
    message(paste(emo::ji("hourglass"), "Filtering time:", time.taken, "secs"))
  }
  message("\n")
  return(output)
}









