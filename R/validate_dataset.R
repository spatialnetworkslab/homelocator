#' Validate the input dataset 
#' 
#' To make sure the input dataset contains all three necessary variables: user, location and timestamp
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of column that holds specific timestamp for each data point and it should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param keep_other_vars Option to keep or remove other variables of the input dataset
#' 

validate_dataset <- function(df, user = "u_id", timestamp = "created_at", location = "grid_id", keep_other_vars = F){
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  if (!rlang::has_name(df, timestamp)) {
    stop(paste(emo::ji("bomb"), "Timestamp column does not exist!"))
  }
  if (!rlang::has_name(df, location)) {
    stop(paste(emo::ji("bomb"), "Location column does not exist!"))
  }

  user <- rlang::sym(user)
  timestamp <- rlang::sym(timestamp)
  location <- rlang::sym(location)

  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }

  if (!is(df %>% pull({{timestamp}}), "POSIXct")) {
    stop("Timestamp is not of class POSIXct")
  }

  unique_users <- df %>% pull({{user}}) %>% n_distinct()
  message(paste(emo::ji("tada"), "Congratulations!! Your dataset has passed validation."))
  message(paste(emo::ji("bust_in_silhouette"), "There are", unique_users, "unique users in your dataset. Now you can start your journey identifying their meaningful location(s)!"))
  message(paste(emo::ji("clap"), "Good luck!"))

  if(keep_other_vars) {
    df
  } else {
    df %>% dplyr::select(c({{user}}, {{location}}, {{timestamp}}))
  }
}






