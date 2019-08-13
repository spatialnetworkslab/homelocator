#' remove bots 
#' 
#' remove certain percentage of top users to avoid bots 
#' 
#' @param df A dataframe with columns for the user id, counts point per user 
#' @param user Name of column that holds unique identifier for each user
#' @param counts Number of data points per user
#' @param top_user_percent The percentage of top user you want to remove 

remove_bots <- function(df, user = "u_id", counts = "n_tweets", top_u_percent = 0.01){
  
  top_u_percent_enq <- enquo(top_u_percent)
  
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  if (!rlang::has_name(df, counts)) {
    stop("Counts per user column does not exist")
  }
  
  user <- rlang::sym(user) 
  counts <- rlang::sym(counts)
  
  n_users <- df %>% pull(!!user) %>% dplyr::n_distinct()
  message(paste(emo::ji("hammer_and_wrench"), "Removing top 1% active users..."))
  
  suppressMessages(output <- df %>% 
    dplyr::select(!!user, !!counts) %>%
    arrange(desc(!!counts)) %>% 
    unique() %>%
    dplyr::slice(round(n_users*!!top_u_percent_enq):n()) %>%
    left_join(., df))
  
  left_users <- output %>% pull(!!user) %>% n_distinct()
  n_rm <- n_users - left_users
  message(paste(emo::ji("white_check_mark"), "Remove", n_rm, "active users, and there are", left_users, "users left."))
  output
}
