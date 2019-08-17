#' extract location 
#' 
#' Extract most likely home location of each user 
#' @param df A nested dataframe by user 
#' @param score_var A scored variable 
extract_home <- function(df, keep_score = F, ...){

  arrange_vars_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data", names(df))])
  
  get_loc_with_progress <- function(data){
    pb$tick()$print()
    get_loc <- data %>%
      dplyr::arrange(desc(!!!arrange_vars_enq)) %>%
      unique() %>%
      slice(1:2) %>%
      dplyr::select(-c(!!!arrange_vars_enq)) %>% 
      setNames(c("home")) %>% 
      pull(home) %>% 
      paste(., collapse = "; ")
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  message(paste(emo::ji("hammer_and_wrench"), "Identifying homes..."))
  
  if(keep_score){
    output <- df %>%
      mutate(home = purrr::map(df[[nested_data]], get_loc_with_progress)) %>%
      unnest(home) 
    n_user <- nrow(output)
    message(paste0("\n", emo::ji("tada"), "Congratulations!! Your have found ", n_user, " users' potential home(s)."))
    output
  } else{
    output <- df %>%
      mutate(home = purrr::map(df[[nested_data]], get_loc_with_progress)) %>%
      dplyr::select(-nested_data) %>%
      unnest(home)
    n_user <- nrow(output)
    message("\n")
    message(paste0(emo::ji("tada"), "Congratulations!! Your have found ", n_user, " users' potential home(s)."))
    output
  }
}



  