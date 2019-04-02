#' arrange 
#' arrange order by certain variable 
#' @param df A nested dataframe 
arrange_var <- function(df, ...){
  arrange_exp_enq <- enquos(...)
  
  df %>% 
    arrange(desc(!!!arrange_exp_enq))
}


#' arrange 
#' arrange order by certain variable 
#' @param df A nested dataframe 
#' @param group_var The varaible to be grouped 
arrange_groupVar <- function(df, group_var, ...){
  group_var_enq <- enquo(group_var)
  arrange_exp_enq <- enquos(...)
  df %>% 
    group_by(!!group_var_enq) %>% 
    arrange(desc(!!!arrange_exp_enq)) 
}
