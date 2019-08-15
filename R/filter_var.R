#' Filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 
#' @param filter_exp A certain condition to meet 
#' @param user Name of column that holds unique identifier for each user
filter_var <- function(df, filter_exp, user = "u_id"){
  
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  
  user <- rlang::sym(user) 
  filter_exp_enq <- enquo(filter_exp)
  
  n_users <- df %>% pull(!!user) %>% dplyr::n_distinct()
  message(paste(emo::ji("locked"), "There are", n_users, "users at this moment."))
  
  output <- df %>% 
    filter(!!filter_exp_enq)
  
  left_users <- output %>% pull(!!user) %>% n_distinct()
  n_rm <- n_users - left_users
  message(paste(emo::ji("white_check_mark"), "Filter out", n_rm, "users, and there are", left_users, "users left.\n"))
  output
}



#' Filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 

filter_in_nest <- function(df, ...){
  
  filter_exp_enq <- enquos(...)
  
  nested_data <- names(df[,grepl("data", names(df))])
  user_data <- df[[nested_data]]
  
  
  filter_with_progress <- function(data){
    pb$tick()$print()
    to_filter <- data %>% 
      filter(!!!filter_exp_enq)
  }
  
  n_users <- nrow(df)
  message(paste(emo::ji("locked"), "There are", n_users, "users at this moment."))
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Filtering...")) 
  output <- df %>% 
    mutate(!!nested_data := purrr::map(df[[nested_data]], ~filter_with_progress(.))) %>% 
    unnest()
  
  left_users <- nrow(output)
  n_rm <- n_users - left_users
  message(paste(emo::ji("white_check_mark"), "Filter out", n_rm, "users, and there are", left_users, "users left.\n"))
  output
}









