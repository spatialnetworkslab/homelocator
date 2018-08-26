#' pull location
#' 
#' pull the location with the maximum score
#' @param data A dataframe with columns for the location, variable score and home location score

pull_homeloc <- function(data, location = "GEOID"){
  if (!rlang::has_name(df, location)) {
    stop("Location column does not exist")
  }
  location <- rlang::sym(location)
  
  data %>% 
    top_n (n=1, wt = hl_score) %>%
    slice(1) %>%
    pull(!!location)
}


#' Extract home location
#' 
#' Estimate a user's home location according to the maximun socre calculated by different varialbes. 
#' @param df A dataframe with columns for the user id, location, variable score and home location score

homeloc_extract <- function(df, user = "u_id", location = "GEOID"){
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  if (!rlang::has_name(df, location)) {
    stop("Location column does not exist")
  }
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  
  df %>% 
    group_by(!!user) %>% 
    tidyr::nest() %>% 
    mutate(homeloc = furrr::future_map_chr(data, pull_homeloc)) %>% 
    select(-data)
}









