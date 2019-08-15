#' score of variables 
#' 
#' Add score of each variables 
#' @param df A nested dataframe by user
# score_user <- function(df, group_var, ...){
#   
#   expr <- enquo(group_var)
#   nested_name <- paste0(quo_name(expr), "_data")
#   
#   adds_exp_enq <- enquos(..., .named = TRUE)
#   
#   nested_data <- names(df)[2]
#   
#   add_column <- . %>% 
#     mutate(!!!adds_exp_enq)
#   
#   df %>% 
#     mutate(adds = purrr::map(df[[nested_data]], add_column)) %>% 
#     unnest(adds) %>% 
#     group_by(!!expr) %>%
#     nest(.key = !!nested_name) 
# }

#' sum scored variables 
#' 
#' Sum up scored variables 
#' @param df A  dataframe 
sum_score <- function(df, ...){
  
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  df %>% 
    unnest() %>% 
    select(!!!adds_exp_enq) %>% 
    unique()  %>% 
    add_col(score = rowSums(.[, -c(1:2)])) 
}





