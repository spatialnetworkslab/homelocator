#' Extract identified home locations for users
#'
#' Extract the most likely home location for each user based on choosed column value
#' @param df A nested dataframe by user
#' @param show_n_loc Number of homes to be shown
#' @param keep_score Choice to keep score or not
#' @param score_var Name of column that holds weighted score for each user
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#'
#' @importFrom rlang has_name
#' @importFrom emo ji
#' @importFrom dplyr progress_estimated
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @importFrom purrr map_chr
#'
#'
#' @export
extract_location <- function(df, user = "u_id", location = "loc_id", show_n_loc = 1, keep_score = F, ...) {
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }

  user <- rlang::sym(user)
  location <- rlang::sym(location)
  var_expr <- enquos(..., .named = TRUE)
  colname_nested_data <- names(df[, grepl("^data$", names(df))])

  get_loc_with_progress <- function(data) {
    pb$tick()$print()
    get_loc <- data %>%
      dplyr::arrange(!!!var_expr) %>%
      slice(1:show_n_loc) %>%
      pull({{ location }})
    if (show_n_loc == 1) {
      get_loc
    } else {
      paste(get_loc, collapse = "; ")
    }
  }
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))

  start.time <- Sys.time()
  message(paste(emo::ji("hammer_and_wrench"), "Start extracting homes for users..."))

  if (keep_score) {
    output <- df %>%
      dplyr::mutate(home = purrr::map_chr(df[[colname_nested_data]], ~ get_loc_with_progress(.)))
  } else {
    output <- df %>%
      dplyr::mutate(home = purrr::map_chr(df[[colname_nested_data]], ~ get_loc_with_progress(.))) %>%
      dplyr::select(-colname_nested_data) %>%
      dplyr::select({{ user }}, home)
  }
  end.time <- Sys.time()
  time.taken <- difftime(end.time, start.time, units = "secs") %>% round(., 3)

  n_user <- output %>%
    pull(!!user) %>%
    n_distinct()
  message("\n")
  message(paste(emo::ji("tada"), "Congratulations!! Your have found", n_user, "users' potential home(s)."))

  if (time.taken > 60) {
    time.taken <- round(time.taken / 60, 2)
    message(paste(emo::ji("hourglass"), "Extracting time:", time.taken, "mins"))
  } else {
    message(paste(emo::ji("hourglass"), "Extracting time:", time.taken, "secs"))
  }
  message("\n")
  return(output)
}
