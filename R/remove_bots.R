#' remove bots 
#' 
#' remove certain percentage of top users to avoid bots 
#' 
#' @param df A dataframe with columns for the user id, counts point per user 
#' @param user Name of column that holds unique identifier for each user
#' @param counts Number of data points per user
#' @param top_user_percent The percentage of top user you want to remove 

remove_bots <- function(df, user = "u_id", counts = "counts_per_user", top_user_percent){
  
  top_user_percent_enq <- enquo(top_user_percent)
  
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  if (!rlang::has_name(df, counts)) {
    stop("Counts per user column does not exist")
  }
  
  user <- rlang::sym(user) 
  counts <- rlang::sym(counts)
  
  num_user <- df %>% pull(!!user) %>% dplyr::n_distinct()
  
  df %>% 
    dplyr::select(!!user, !!counts) %>%
    unique() %>%
    dplyr::slice(round(num_user*!!top_user_percent_enq):n()) %>%
    left_join(., df) %>% 
    unnest()
}
