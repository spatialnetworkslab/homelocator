#' Filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 
#' @param filter_exp A certain condition to meet 
#' @param user Name of column that holds unique identifier for each user
#' 
#' 
#' 
filter_verbose <- function(df, filter_exp, user = "u_id"){
  
  if (!rlang::has_name(df, user)) {
    stop(paste(emo::ji("bomb"), "User column does not exist!"))
  }
  
  user <- rlang::sym(user) 
  filter_exp_enq <- enquo(filter_exp)
  
  n_users <- df %>% pull({{user}}) %>% dplyr::n_distinct()
  message("\n")
  message(paste(emo::ji("locked"), "There are", n_users, "users at this moment."))
  
  output <- df %>% 
    filter({{filter_exp_enq}})
  
  left_users <- output %>% pull({{user}}) %>% n_distinct()
  n_rm <- n_users - left_users
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Filter out", n_rm, "users, and there are", left_users, "users left.\n"))
  output
}



#' Filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 
#' @param ... Variables or functions 
#' 
#' 
filter_nested <- function(df, ...){
  
  filter_exp_enq <- enquos(...)
  nested_data <- names(df[,grepl("data", names(df))])
  user_data <- df[[nested_data]]
  
  #filter
  filter_with_progress <- function(data){
    pb$tick()$print()
    to_filter <- data %>% 
      filter(!!!filter_exp_enq)
  }
  
  n_users <- df[1] %>% dplyr::n_distinct()
  message(paste(emo::ji("locked"), "There are", n_users, "users at this moment."))
  message(paste(emo::ji("hammer_and_wrench"), "Filtering...")) 
  
  pb <- dplyr::progress_estimated(length(user_data))
  #map filter on nested data
  output <- df %>% 
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~filter_with_progress(.))) 
  
  output_data <- output[[nested_data]]
  
  #check empty tibble 
  result <- output %>% 
    filter(!(purrr::map_lgl(output_data, plyr::empty)))
    # mutate(empty_tb = purrr::map_lgl(output_data, plyr::empty)) %>% 
    # filter(empty_tb != T) %>% 
    # dplyr::select(-empty_tb)
  
  left_users <- result[1] %>% dplyr::n_distinct()
  n_rm <- n_users - left_users
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Filter out", n_rm, "users, and there are", left_users, "users left.\n"))
  result
}








