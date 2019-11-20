#' extract location 
#' 
#' Extract most likely home location of each user 
#' @param df A nested dataframe by user 
#' @param show_n_home Number of homes to be shown 
#' @param keep_score Choice to keep score or not 
#' 
extract_home <- function(df, ..., show_n_home = 1, keep_score = F){
  df <- df %>% ungroup()
  arrange_vars_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data", names(df))])
  
  get_loc_with_progress <- function(data){
    pb$tick()$print()
    get_loc <- data %>%
      ungroup() %>% 
      dplyr::arrange(desc(!!!arrange_vars_enq)) %>%
      unique() %>%
      slice(1:show_n_home) %>%
      dplyr::select(-c(!!!arrange_vars_enq)) %>% 
      setNames(c("home")) %>% 
      pull(home) %>% 
      paste(., collapse = "; ")
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Identifying homes..."))
  
  if(keep_score){
    output <- df %>%
      mutate(home = purrr::map(df[[nested_data]], get_loc_with_progress)) %>%
      unnest(home) 
    n_user <- nrow(output)
    message("\n")
    message(paste0(emo::ji("tada"), "Congratulations!! Your have found ", n_user, " users' potential home(s)."))
  } else{
    output <- df %>%
      mutate(home = purrr::map(df[[nested_data]], get_loc_with_progress)) %>%
      dplyr::select(-nested_data) %>%
      unnest(home)
    n_user <- nrow(output)
    message("\n")
    message(paste0(emo::ji("tada"), "Congratulations!! Your have found ", n_user, " users' potential home(s)."))
  }
  output
}
