#' score of variables 
#' 
#' Add score of each variables 
#' @param df A nested dataframe by user
#' @param group_var Variable to be grouped 
#' @param user Name of column that holds unique identifier for each user
#' @param location Name of column that holds unique identifier for each location
#' 
#' 
score_var <- function(df, group_var, user = "u_id", location = "grid_id", keep_original_vars = F, ...){
  expr <- enquo(group_var)
  adds_exp_enq <- enquos(..., .named = TRUE)
  nested_name <- paste0(quo_name(expr), "_data")
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  if (!rlang::has_name(df, location)) {
    stop(paste(emo::ji("bomb"), "Location column does not exist!"))
  }
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  
  df_nest <- df %>%
    group_by(!!expr) %>%
    nest(.key = !!nested_name) 
  
  user_data <- df_nest[[nested_name]]
  
  transmute_with_progress <- function(data){
    pb$tick()$print()
    transmute_column <- data %>% 
      transmute(!!!adds_exp_enq)
  }
  
  add_with_progress <- function(data){
    pb$tick()$print()
    add_column <- data %>% 
      mutate(!!!adds_exp_enq)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Scoring variables..."))
  
  if(keep_original_vars){
    output <- df_nest %>% 
      mutate(!!nested_name := purrr::map(df_nest[[nested_name]], ~add_with_progress(.))) %>% 
      # mutate(!!nested_data := purrr::map(df_nest[[nested_data]], ~add_with_progress(.))) %>% 
      unnest()
    ori_cols <- names(df)
    new_cols <- names(output)
    added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
    message(paste("\n", emo::ji("white_check_mark"), "New added variables:", added_cols))
    output
  }else{
    adds <- do.call(rbind, purrr::map(df_nest[[nested_name]], ~transmute_with_progress(.)))
    output <- df %>%
      dplyr::select(c(!!user, !!location)) %>% 
      dplyr::bind_cols(adds)
    added_cols <- names(adds) %>% paste(., collapse = ", ")
    message("\n")
    message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
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
  
  if (!rlang::has_name(df, location)) {
    stop(paste(emo::ji("bomb"), "Location column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  df_sub <- df %>% 
    dplyr::select(c(!!user, !!location, !!!adds_exp_enq)) %>% 
    replace(., is.na(.), 0)
  
  df_nest <- df_sub %>%
    group_by(!!user) %>%
    nest()
  
  sum_score_with_progress <- function(data){
    pb$tick()$print()
    loc_index <- which(colnames(data)==location)
    loc <- data %>% 
      dplyr::select(!!location)
    sum_score <- data %>%
      transmute(score = rowSums(.[ , -c(loc_index)]))
    loc_score <- bind_cols(loc, sum_score)
  }
   
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df_nest))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Sum scores..."))
  
  df_nest %>% 
    mutate(data = purrr::map(data, ~sum_score_with_progress(.)))
}





