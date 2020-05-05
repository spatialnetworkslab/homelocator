#' remove bots 
#' 
#' remove certain percentage of top users to avoid bots 
#' 
#' @param df A dataframe with columns for the user id, counts point per user 
#' @param user Name of column that holds unique identifier for each user
#' @param counts Number of data points per user
#' @param topNpct_user The percentage of active users 

remove_top_users <- function(df, user = "u_id", counts = "n_tweets", topNpct_user = 0.01){
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  if (!rlang::has_name(df, counts)) {
    stop(paste(emo::ji("bomb"), "The column of tweets counts for each user does not exist!"))
  }
  
  user <- rlang::sym(user) 
  counts <- rlang::sym(counts)
  
  n_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  message(paste(emo::ji("locked"), "There are", n_users, "users at this moment."))
  message(paste0(emo::ji("hammer_and_wrench"), " Removing top ", topNpct_user*100, "% active users..."))
  
  output <- df %>% 
    dplyr::select({{user}}, {{counts}}) %>%
    arrange(desc({{counts}})) %>% 
    unique() %>%
    dplyr::slice(round(n_users*topNpct_user):n()) %>%
    left_join(., df)
  
  left_users <- output %>% pull({{user}}) %>% n_distinct()
  n_rm <- n_users - left_users
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Removed", n_rm, "active users, and there are", left_users, "users left."))
  output
}
