#' score of variables 
#' 
#' Add score of each variables 
#' @param df A nested dataframe by user
#' @param group_var Variable to be grouped 
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#' @param keep_original_vars Choice to keep original variables or not
#' 
score_var <- function(df, user = "u_id", location = "grid_id", keep_ori_vars = F, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  if (!rlang::has_name(df, location)) {
    stop(paste(emo::ji("bomb"), "Location column does not exist!"))
  }
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  
  df_nest <- df %>%
    nest_legacy(-({{user}}))
  
  nested_data_nm <- names(df_nest[,grepl("data$", names(df_nest))])
  user_data <- df_nest[[nested_data_nm]]
  
  transmute_with_progress <- function(data){
    pb$tick()$print()
    transmute_column <- data %>% 
      transmute(!!!adds_exp_enq)
    
    data %>% 
      dplyr::select({{location}}) %>% 
      bind_cols(transmute_column)
  }
  
  add_with_progress <- function(data){
    pb$tick()$print()
    add_column <- data %>% 
      mutate(!!!adds_exp_enq)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Scoring variables..."))
  
  if(keep_ori_vars){
    output <- df_nest %>% mutate({{nested_data_nm}} := purrr::map(df_nest[[nested_data_nm]], ~add_with_progress(.)))
    
    ori_cols <- df_nest[[nested_data_nm]][[1]] %>% names()
    new_cols <- output[[nested_data_nm]][[1]] %>% names()
    added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
    message(paste("\n", emo::ji("white_check_mark"), "Scored variables:", added_cols))
    output
  }else{
    output <- df_nest %>% mutate({{nested_data_nm}} := purrr::map(df_nest[[nested_data_nm]], ~transmute_with_progress(.)))
    add_cols <- output[[nested_data_nm]][[1]] %>% names()
    added_cols <- add_cols[-which(add_cols == location)] %>% paste(., collapse = ", ")
    message("\n")
    message(paste(emo::ji("white_check_mark"), "Scored variables:", added_cols))
    output
  }
}


#' sum scored variables 
#' 
#' Sum up scored variables 
#' @param df A  dataframe 
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#' 
#' 
#' 
sum_score <- function(df, user = "u_id", location = "grid_id", ...){
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  nested_data_nm <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data_nm]]
  
  sum_score_with_progress <- function(data){
    pb$tick()$print()
    data_sub <- data %>% dplyr::select(c({{location}}, !!!adds_exp_enq)) 
    
    loc_index <- which(colnames(data_sub) == location)
    loc <- data_sub %>% dplyr::select({{location}})
    sum_score <- data_sub %>% mutate(score = rowSums(.[ , -c(loc_index)]))
    sum_score
  }
   
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Sum scores..."))
  
  df %>% mutate(data = purrr::map(user_data, ~sum_score_with_progress(.)))
}


