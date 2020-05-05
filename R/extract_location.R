#' extract location 
#' 
#' Extract most likely home location of each user 
#' @param df A nested dataframe by user 
#' @param show_n_loc Number of homes to be shown 
#' @param keep_score Choice to keep score or not 
#' @param score_var Name of column that holds weighted score for each user 
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#' 

extract_location <- function(df, score_var, user = "u_id", location = "grid_id", show_n_loc = 1, keep_score = F){
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  nested_data <- names(df[,grepl("data", names(df))])
  
  get_loc_with_progress <- function(data){
    pb$tick()$print()
    get_loc <- data %>%
      dplyr::arrange(desc(!!!score_var)) %>% 
      slice(1:show_n_loc) %>%
      pull({{location}}) 
    if(show_n_loc == 1){
      get_loc
    } else{
      paste(get_loc, collapse = "; ")
    }
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Identifying homes..."))
  
  if(keep_score){
    output <- df %>%
      mutate(home = purrr::map(df[[nested_data]], get_loc_with_progress)) %>%
      unnest(home) 
    n_user <- output %>% pull(!!user) %>% n_distinct()
    message("\n")
    message(paste0(emo::ji("tada"), "Congratulations!! Your have found ", n_user, " users' potential home(s)."))
  } else{
    output <- df %>%
      mutate(home = purrr::map(df[[nested_data]], get_loc_with_progress)) %>%
      dplyr::select(-nested_data) %>%
      unnest(home) %>% 
      dplyr::select({{user}}, home)
    n_user <- output %>% pull(!!user) %>% n_distinct()
    message("\n")
    message(paste0(emo::ji("tada"), "Congratulations!! Your have found ", n_user, " users' potential home(s)."))
  }
  output
}
