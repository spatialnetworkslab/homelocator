#' Nest dataframe 
#' 
#' Nest dataframe by variable 
#' @param df A dataframe 
#' @param group_var variable to be grouped by 
nest_dataframe <- function(df, group_var) {
  if (!is.data.frame(df)) {
    stop("Error: Dataset is not a dataframe")
  }
  
  expr <- enquo(group_var)
  
  cat(paste("Nesting the dataset by", quo_name(expr), "..."))
  
  nested_name <- paste0(quo_name(expr), "_data")
  
  df %>%
    group_by(!!expr) %>%
    nest(.key = !!nested_name) 
}
