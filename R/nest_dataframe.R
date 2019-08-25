#' Nest dataframe 
#' 
#' Nest dataframe by variable
#' @param df A dataframe 
#' @param group_var Variable to be grouped 
nest_by_sglGp <- function(df, group_var) {
  if (!is.data.frame(df)) {
    stop("Error: Dataset is not a dataframe")
  }
  
  expr <- rlang::sym(group_var)
  message(paste(emo::ji("hammer_and_wrench"), "Nesting the dataset by", quo_name(expr), "..."))
  nested_name <- paste0(quo_name(expr), "_data")
  df %>%
    group_by(!!expr) %>%
    nest(.key = !!nested_name) 
}


#' Nest dataframe 
#' 
#' Nest dataframe by multiple varibles 
#' @param df A dataframe 
#' @param group_vars A list contains variables to be grouped
#'
nest_by_mulGps <- function(df, group_vars) {
  if (!is.data.frame(df)) {
    stop("Error: Dataset is not a dataframe")
  }
  
  stopifnot(
    is.list(group_vars)
  )
  
  message(paste(emo::ji("hammer_and_wrench"), "Nesting dataset by multiple groups..."))

  df %>%
    group_by(!!!group_vars) %>%
    nest() 
}

