#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
add_col <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  df %>% 
    mutate(!!!adds_exp_enq)
}



#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe 
#' @param group_var The variable to be grouped 
#' @param mutate_vars The variables you want to add to the dataframe

add_groupCol <- function(df, group_vars, mutate_vars){
  
  stopifnot(
    is.list(group_vars),
    is.list(mutate_vars)
  )
  
  df %>%
    group_by(!!!group_vars) %>%
    mutate(!!!mutate_vars) %>%
    ungroup() %>% 
    unnest() 
}
