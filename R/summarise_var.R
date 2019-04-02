#' Computed variables
#' 
#' Add needed variables as you want 
#' @param df A nested dataframe 
summarise_var <- function(df, ...){

  if(!is.list(df[,grepl("data", names(df))]))
    stop("Error: Dataset is not nested!")
  
  adds_exp_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data", names(df))])
  
  add_column <- . %>% 
    summarise(!!!adds_exp_enq)
  
  df %>%
    mutate(adds = purrr::map(df[[nested_data]], add_column)) %>%
    unnest(adds)
}

#' Computed variables by group
#' 
#' Add needed variables by group 
#' @param df A nested dataframe 

summarise_groupVar <- function(df, group_vars, summary_vars){
  
  stopifnot(
    is.list(group_vars),
    is.list(summary_vars),
    is.list(df[,grepl("data", names(df))])
  )
  
  cal_column <- . %>% 
    summarise(!!!summary_vars)
  
  add_column <- . %>% 
    mutate(adds = purrr::map(data, cal_column))
  
  nested_data <- names(df[,grepl("data", names(df))])
  
  # double nest 
  df[[nested_data]] <- purrr::map(df[[nested_data]], ~.x %>% group_by(!!!group_vars) %>% nest())
  
  df %>%
    mutate(vars = purrr::map(df[[nested_data]], add_column)) %>%
    unnest(vars) %>%
    unnest(adds)
}
