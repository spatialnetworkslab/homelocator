#' Give a weighted value for one or more variables in a nested dataframe
#' 
#' Give a weighted value for one or more variables in a nested dataframe
#' @param df A nested dataframe by user
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#' @param keep_ori_vars Option to keep or drop original varialbes 
#' @param ... Name-value pairs of expression
#' 
#' 
#' Can we replace this with just mutate_nested? And potentially add transmute_nested counterpart?
#' 
score_nested <- function(df, user = "u_id", location = "loc_id", keep_original_vars = F, ...){
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  if (!rlang::has_name(df, location)) {
    stop(paste(emo::ji("bomb"), "Location column does not exist!"))
  }
  
  var_expr <- enquos(..., .named = TRUE)
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  
  df_nest <- df %>%
    nest_legacy(-({{user}}))
  
  colname_nested_data <- names(df_nest[ , grepl("^data$", names(df_nest))])
  
  transmute_with_progress <- function(data){
    pb$tick()$print()
    transmute_column <- data %>% 
      transmute(!!!var_expr)
    
    data %>% 
      dplyr::select({{location}}) %>% 
      bind_cols(transmute_column)
  }
  
  add_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      mutate(!!!var_expr)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
 
  
  message(paste(emo::ji("hammer_and_wrench"), "Start scoring ..."))
  start.time <- Sys.time()
  if(keep_original_vars){
    output <- df_nest %>% 
      mutate({{colname_nested_data}} := purrr::map(df_nest[[colname_nested_data]], ~add_with_progress(.)))
  }else{
    output <- df_nest %>% 
      mutate({{colname_nested_data}} := purrr::map(df_nest[[colname_nested_data]], ~transmute_with_progress(.)))
  }
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  colnames_original <- df_nest[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish scoring! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  message(paste(emo::ji("hourglass"), "Scoring time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}


#' Summarises all scored columns and return one single summary score per row
#' 
#' summarises all scored columns and return one single summary score per row
#' @param df A  dataframe 
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#' @param ... A selection of columns to sum
#' 
#' 
score_summary <- function(df, user = "u_id", location = "loc_id", ...){
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  var_expr <- enquos(...)
  
  colname_nested_data <- names(df[,grepl("^data$", names(df))])
 
  sum_score_with_progress <- function(data){
    pb$tick()$print()
    data_sub <- data %>% dplyr::select({{location}}, !!!var_expr) 
    
    location_index <- which(colnames(data_sub) == location)
    data_sub %>% 
      mutate(score = rowSums(.[ , -c(location_index)]))
  }
   
  # create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  
  message(paste(emo::ji("hammer_and_wrench"), "Start summing scores..."))
  start.time <- Sys.time()
  output <- df %>% 
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~sum_score_with_progress(.)))
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  colnames_original <- df[[colname_nested_data]][[1]] %>% names()
  colnames_new <- output[[colname_nested_data]][[1]] %>% names()
  colnames_added <- dplyr::setdiff(colnames_new, colnames_original)
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish summing! There are", length(colnames_added), "new added variables:", paste(colnames_added, collapse = ", ")))
  message(paste(emo::ji("hourglass"), "Summing time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}


