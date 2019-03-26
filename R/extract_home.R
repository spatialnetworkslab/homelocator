#' extract location 
#' 
#' Extract most likely home location of each user 
#' @param df A nested dataframe by user 
#' @param score_var A scored variable 
extract_home <- function(df, score_var, ...){
  score_var_enq <- enquo(score_var)
  arrange_vars_enq <- enquos(...)
  nested_data <- names(df)[2]
  
  get_loc <- . %>%
    .[!duplicated(.$GEOID), ] %>% 
    dplyr::arrange(desc(!!!arrange_vars_enq)) %>%
    unique() %>%
    top_n(5) %>%
    filter(!!score_var_enq) %>%
    slice(1:2) %>%
    pull(GEOID) %>%
    paste(., collapse = "; ")
  
  df %>%
    mutate(home = purrr::map(df[[nested_data]], get_loc)) %>%
    select(-nested_data) %>%
    unnest(home)
}



  