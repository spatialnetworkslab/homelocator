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
  
  nested_data <- names(df_nest[,grepl("data", names(df_nest))])
  user_data <- df_nest[[nested_data]]
  
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
      mutate(!!nested_data := purrr::map(df_nest[[nested_data]], ~add_with_progress(.))) %>% 
      unnest()
    ori_cols <- names(df)
    new_cols <- names(output)
    added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
    message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
    output
  }else{
    adds <- do.call(rbind, purrr::map(df_nest[[nested_data]], ~transmute_with_progress(.)))
    output <- df %>%
      dplyr::select(c(!!user, !!location)) %>% 
      dplyr::bind_cols(adds)
    added_cols <- names(adds) %>% paste(., collapse = ", ")
    message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
    output
  }
  
  # ori_cols <- df[[nested_data]][[1]] %>% names()
  # new_cols <- output[[nested_data]][[1]] %>% names()
  # added_cols <- dplyr::setdiff(new_cols, ori_cols) %>% paste(., collapse = ", ")
  # message(paste(emo::ji("white_check_mark"), "New added variables:", added_cols))
  # output
}
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





