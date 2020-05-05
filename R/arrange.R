#' arrange in nested dataframe 
#' @param df A nested dataframe 
#' @param group_var The variable to be grouped 
#' @param ... Variables or functions 
#' 
#' 
arrange_nested <- function(df, group_var, ...){
  group_var_enq <- rlang::sym(group_var)
  arrange_exp_enq <- enquos(...)
  
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]
  
  arrange_with_progress <- function(data){
    pb$tick()$print()
    arrange_column <- data %>% 
      group_by(!!group_var_enq) %>% 
      arrange(desc(!!!arrange_exp_enq)) %>% 
      ungroup()
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Sorting..."))
  
  output <- df %>%
    mutate(!!nested_data := purrr::map(df[[nested_data]], ~arrange_with_progress(.))) 
}
  
  
 