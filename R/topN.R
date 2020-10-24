#' Select top n rows by certain value 
#' 
#' Select top n rows in each group, ordered by wt within a nested dataframe
#' @param df A nested dataframe 
#' @param n Number of rows to return 
#' @param wt The variable to use for ordering 
#' 
#' @importFrom dplyr progress_estimated
#' @importFrom rlang sym
#' @importFrom emo ji
#' @export
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
  time.taken <-  difftime(end.time, start.time, units = "secs") %>% round(., 3)
  
  message("\n")
  message(paste(emo::ji("white_check_mark"), "Finish selecting top", n, "row(s)!"))
  
  if(time.taken > 60){
    time.taken <- round(time.taken/60, 2)
    message(paste(emo::ji("hourglass"), "Selecting time:", time.taken, "mins"))
  } else{
    message(paste(emo::ji("hourglass"), "Selecting time:", time.taken, "secs"))
  }
  message("\n")
  
  return(output)
}







