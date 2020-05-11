#' Remove users
#' 
#' Remove certain percent of top active users based on the total number of data points  
#' 
#' @param df A dataframe with columns for the user id, counts point per user 
#' @param user Name of column that holds unique identifier for each user
#' @param counts Name of column that holds the data points frequency for each user         
#' @param topNpct_user A decimal number that represent the certain percentage of users to remove

remove_top_users <- function(df, user = "u_id", counts = "n_points", topNpct_user = 1){
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  if (!rlang::has_name(df, counts)) {
    stop(paste(emo::ji("bomb"), "The column of counts of data points for each user does not exist!"))
  }
  
  user <- rlang::sym(user) 
  counts <- rlang::sym(counts)
  
  n_original_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_original_users, "users at this moment."))
  message(paste(emo::ji("hammer_and_wrench"), "Removing top", topNpct_user, "% active users..."))
  
  output <- df %>% 
    arrange(desc({{counts}})) %>% 
    dplyr::slice(round(n_original_users*(topNpct_user/100)):n()) 
  
  n_new_users <- output %>% pull({{user}}) %>% n_distinct()
  n_removed_users <- n_original_users - n_new_users
  message(paste(emo::ji("white_check_mark"), "Removed", n_removed_users, "active users!"))
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_new_users, "users left."))
  
  return(output)
}


