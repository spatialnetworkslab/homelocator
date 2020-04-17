#' Nest dataframe 
#' 
#' Nest dataframe by variable
#' @param df A dataframe 
#' @param ... Variables or functions 
#' 
#' 
nest_cols <- function(df, ...){
  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }
  
  nest_exp_enq <- enquos(..., .named = TRUE)
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Start nesting..."))
  df %>%
    nest_legacy(!!!nest_exp_enq)
}

#' Nest dataframe 
#' @param df A dataframe 
#' @param ... Variables or functions 
#' 
#' 
nest_cols_in_nest <- function(df, ...){
  if(!is.list(df[,grepl("data", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }
  
  nest_cols <- enquos(..., .named = TRUE)
  nested_data <- names(df[,grepl("data", names(df))])
  user_data <- df[[nested_data]]

  nest_with_progress <- function(data){
    pb$tick()$print()
    suppressWarnings(
      unnest_col <- data %>%
        nest_legacy(!!!nest_cols)
      )
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message(paste(emo::ji("hammer_and_wrench"), "Nesting..."))
  output <- df %>%
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~nest_with_progress(.)))
  output
}


#' Unnest dataframe
#' 
#' @param df A dataframe
#' @param ... Variables or functions 
#' 
#' 
unnest_cols <- function(df, ...){
  if (!is.data.frame(df)) {
    stop(paste(emo::ji("bomb"), "Dataset is not a dataframe!"))
  }
  
  nest_exp_enq <- enquos(..., .named = TRUE)
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Unnesting..."))
  
  suppressWarnings(df %>%
    unnest_legacy(!!!nest_exp_enq))
}


#' Nunest dataframe
#' 
#' @param df A nested dataframe
#' @param ... Variables or functions 
#' 
#' 
#' 
unnest_cols_in_nest <- function(df, ...){
  if(!is.list(df[,grepl("data", names(df))])){
    stop(paste(emo::ji("bomb"), "Error: Dataset is not nested!"))
  }
  nested_data <- names(df[,grepl("data", names(df))])
  user_data <- df[[nested_data]]
  
  unnest_with_progress <- function(data){
    pb$tick()$print()
    suppressWarnings(unnest_col <- data %>% 
        unnest_legacy())
  }
  #create the progress bar
  pb <- dplyr::progress_estimated(length(user_data))
  message("\n")
  message(paste(emo::ji("hammer_and_wrench"), "Unnesting..."))
  output <- df %>%
    mutate({{nested_data}} := purrr::map(df[[nested_data]], ~unnest_with_progress(.)))
  output
}