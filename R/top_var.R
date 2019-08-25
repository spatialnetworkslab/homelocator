#' select the top entries in each group 
#' 
#' Select the top entries in each group, ordered by wt
#' @param df A dataframe 
#' @param n Number of entries to be selected
#' @param wt The variable to be used for ordering
top_n_in_nest <- function(df, n = 2, wt){
  wt_enq = enquo(wt)
  nested_data <- names(df[,grepl("data$", names(df))])
  user_data <- df[[nested_data]]
  
  top_with_progress <- function(data){
    pb$tick()$print()
    top_var <- data %>% 
      top_n(n = n, wt = !!wt_enq)
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Selecting top entries in each group..."))
  
  output <- df %>%
    mutate(!!nested_data := purrr::map(df[[nested_data]], ~top_with_progress(.))) 
  output
}