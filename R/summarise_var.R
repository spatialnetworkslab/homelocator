#' Computed variables
#' 
#' Add needed variables as you want 
#' @param df A nested dataframe 
summ_in_nest <- function(df, ...){
  if(!is.list(df[,grepl("data", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }

  adds_exp_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]
  ori_cols <- names(df)
  
  # define reading function which includes the progress bar
  summarise_with_progress <- function(data){
    pb$tick()$print()
    summarise_cols <- data %>% 
      summarise(!!!adds_exp_enq)
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Summarise variables in nested dataset..."))
  
  output <- df %>%
    mutate(adds = purrr::map(df[[nested_data]], ~summarise_with_progress(.))) %>%
    unnest_legacy(adds)
  new_cols <- names(output)
  added_cols <- dplyr::setdiff(new_cols, ori_cols) 
  added_cols_nm <- added_cols %>% paste(., collapse = ", ")
  message(paste("\n", emo::ji("white_check_mark"), "After summarising, there are", length(added_cols), "new added variables:", added_cols_nm))
  output
}


#' Computed variables by group
#' 
#' Add needed variables by group 
#' @param df A nested dataframe 
#' @param nest_cols Variables to be nested 
#' @param summary_vars Summarise expression
summ_byGRP_in_nest <- function(df, nest_cols, summary_vars){
  
  if(nrow(df)==0){
    stop(paste(emo::ji("bomb"), "No user left, tune your treshold and try again."))
  }
  
  stopifnot(
    is.list(summary_vars),
    is.list(df[,grepl("data", names(df))])
  )
  
  cal_column <- . %>% 
    summarise(!!!summary_vars)

  add_column <- . %>% 
    mutate(adds = purrr::map(data, cal_column)) %>% 
    unnest_legacy(adds)
  
  nested_data <- names(df[,grepl("data", names(df))])
  ori_cols <- df[[nested_data]][[1]] %>% names()
  
  # double nest 
  df[[nested_data]] <- purrr::map(df[[nested_data]], ~.x %>% nest(data = nest_cols))
  
  output <- df %>%
      mutate({{nested_data}} := purrr::map(df[[nested_data]], add_column)) 
  output_cols <- output[[nested_data]][[1]] %>% names()
  add_cols <- setdiff(output_cols, ori_cols)
  add_cols <- add_cols[-which(add_cols == "data")]
  message(paste(emo::ji("white_check_mark"), "New added variable:", add_cols, "\n"))
  output
}