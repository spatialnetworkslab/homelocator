#' Remove top N percent of active users based on the frequency of data points per user
#'
#' Remove top N percent of active users based on the frequency of data points per user.
#' Although the majority of users are real people, some accounts are run by algorithms or 'bots', whereas others can be considered as spam accounts.
#' Removing a certain top N percent of active users is an oft-used approach to remove such accounts and reduce the number of such users in the final dataset.
#'
#' @param df A dataframe with columns for the user id, counts point per user
#' @param user Name of column that holds unique identifier for each user
#' @param counts Name of column that holds the data points frequency for each user
#' @param topNpct_user A decimal number that represent the certain percentage of users to remove
#' @param rm_topNpct_user Option to remove or keep the top N percent active users
#'
#' @importFrom rlang has_name
#' @importFrom rlang sym
#' @importFrom emo ji
#' @importFrom dplyr n_distinct
#' @importFrom dplyr slice
#'
#' @export
remove_top_users <- function(df, user = "u_id", counts = "n_points", topNpct_user = 1, rm_topNpct_user = F) {
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  if (!rlang::has_name(df, counts)) {
    stop(paste(emo::ji("bomb"), "The column of counts of data points for each user does not exist!"))
  }

  user <- rlang::sym(user)
  counts <- rlang::sym(counts)

  n_original_users <- df %>%
    pull({{ user }}) %>%
    dplyr::n_distinct()
  message(paste(emo::ji("bust_in_silhouette"), "There are", n_original_users, "users at this moment."))

  if (rm_topNpct_user == F) {
    message(paste(emo::ji("bust_in_silhouette"), "Skip removing active users"))
    output <- df %>% arrange(desc({{ counts }}))
  } else {
    message(paste0(emo::ji("hammer_and_wrench"), " Start removing top ", topNpct_user, "% top active users..."))
    start.time <- Sys.time()
    output <- df %>%
      arrange(desc({{ counts }})) %>%
      dplyr::slice(round(n_original_users * (topNpct_user / 100)):n())
    end.time <- Sys.time()
    time.taken <- difftime(end.time, start.time, units = "secs") %>% round(., 3)

    n_new_users <- output %>%
      pull({{ user }}) %>%
      n_distinct()
    n_removed_users <- n_original_users - n_new_users
    message(paste(emo::ji("white_check_mark"), "Finish removing! Removed", n_removed_users, "active users!"))
    message(paste(emo::ji("bust_in_silhouette"), "There are", n_new_users, "users left."))

    if (time.taken > 60) {
      time.taken <- round(time.taken / 60, 2)
      message(paste(emo::ji("hourglass"), "Removing time:", time.taken, "mins"))
    } else {
      message(paste(emo::ji("hourglass"), "Removing time:", time.taken, "secs"))
    }
    message("\n")
  }
  return(output)
}
