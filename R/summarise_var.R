#' Computed variables
#' 
#' Add needed variables as you want 
#' @param df A nested dataframe 
summarise_var <- function(df, ...){

  if(!is.list(df[,grepl("data", names(df))]))
    stop("Error: Dataset is not nested!")
  
  adds_exp_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]
  ori_cols <- names(df)
  
  # define reading function which includes the progress bar
  summarise_with_progress <- function(data){
    pb$tick()$print()
    sum_column <- data %>% 
      summarise(!!!adds_exp_enq)
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Summarising..."))
  
  output <- df %>%
    mutate(adds = purrr::map(df[[nested_data]], ~summarise_with_progress(.))) %>%
    unnest(adds)
  new_cols <- names(output)
  added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
  message("\n")
  message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
  output
}

#' Computed variables by group
#' 
#' Add needed variables by group 
#' @param df A nested dataframe 
#' @param group_vars Variables be grouped 
#' @param summary_vars Variables to be added 
summarise_groupVar <- function(df, group_vars, summary_vars){
  
  stopifnot(
    is.list(group_vars),
    is.list(summary_vars),
    is.list(df[,grepl("data", names(df))])
  )
  
  cal_column <- . %>% 
    summarise(!!!summary_vars)

  add_column <- . %>% 
    mutate(adds = purrr::map(data, cal_column)) %>% 
    unnest(adds)
  
  nested_data <- names(df[,grepl("data", names(df))])
  
  # double nest 
  df[[nested_data]] <- purrr::map(df[[nested_data]], ~.x %>% group_by(!!!group_vars) %>% nest())
  
  output <- df %>%
      mutate(!!nested_data := purrr::map(df[[nested_data]], add_column)) 
    col_na <- output[[nested_data]][[1]] %>% names()
    add_cols <- col_na[3:length(col_na)]
    message("\n")
    message(paste(emo::ji("white_check_mark"), "New added variable:", add_cols, "\n"))
    output
}






