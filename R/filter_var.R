#' Filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 
#' @param filter_exp A certain condition to meet 
filter_var <- function(df, filter_exp){
  filter_exp_enq <- enquo(filter_exp)
  df %>% 
    filter(!!filter_exp_enq)
}



#' Filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 

filter_nest <- function(df, ...){
  
  filter_exp_enq <- enquos(...)
  
  nested_data <- names(df[,grepl("data", names(df))])
  
  to_filter <- . %>% 
    filter(!!!filter_exp_enq)
  
  df %>% 
    mutate(result = purrr::map(df[[nested_data]], to_filter)) 
}
