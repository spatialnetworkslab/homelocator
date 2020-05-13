#' Select top n rows by value 
#' 
#' Select top n rows by value in nested dataframe 
#' @param df A nested dataframe 
#' @param n Number of rows to return 
#' @param wt The variable to use for ordering 
#' 
#' 
top_n_nested <- function(df, n = 2, wt){
  wt_expr <- rlang::sym(wt)
  colname_nested_data <- names(df[ , grepl("^data$", names(df))])
  
  top_n_with_progress <- function(data){
    pb$tick()$print()
    data %>% 
      top_n(n = n, wt = {{wt_expr}})
  }
  
  #create the progress bar
  pb <- dplyr::progress_estimated(nrow(df))
  
  message(paste(emo::ji("hammer_and_wrench"), "Start selecting top", n, "row(s)...")) 
  start.time <- Sys.time()
  output <- df %>%
    mutate({{colname_nested_data}} := purrr::map(df[[colname_nested_data]], ~top_n_with_progress(.))) 
  end.time <- Sys.time()
  time.taken <-  difftime(end.time, start.time, units = "mins") %>% round(., 2)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish selecting top", n, "row(s)!"))
  message(paste(emo::ji("hourglass"), "Selecting time:", time.taken, "mins"))
  message("\n")
  
  return(output)
}







