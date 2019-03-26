.onAttach <- function(libname, pkgname){
  packageStartupMessage("Welcome to my package")
}

.onLoad <- function(libname, pkgname){
  op <- options()
  op.devtools <- list(
    devtools.path = "~/R-dev",
    devtools.install.args = "",
    devtools.name = "your name goes here",
    devtools.desc.author = 'person("First","Second", "first.last@example.com", role = c("aut","cre"))',
    devtools.desc.license = "What license is it under?",
    devtools.desc.suggests = NULL,
    devtools.desc = list()
  )
  toset <- !(names(op.devtools) %in% names(op))
  if(any(toset)) options(op.devtools[toset])
  invisible()
}


#' Basic variables 
#' 
#' Add basic variables derive from timestamp 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param timestamp Name of timestamp column. Should be POSIXct
#' 
#' 
derive_timestamp <- function(df, timestamp){
  timestamp_enq <- rlang::enquo(timestamp)
  
  if (!is.data.frame(df)) {
    stop("Error: Dataset is not a dataframe")
  }
  
  if (!is(df %>% pull(!!timestamp_enq), "POSIXct")) {
    stop("Error: Timestamp is not of class POSIXct")
  }
  
  cat("Deriving basic needed variables from timestamp column of the dataset...")
  
  df %>% 
    mutate(year = lubridate::year(!!timestamp_enq),
           month = lubridate::month(!!timestamp_enq),
           day = lubridate::day(!!timestamp_enq), 
           day_of_week = lubridate::wday(!!timestamp_enq),
           hour_of_day = lubridate::hour(!!timestamp_enq), 
           date = as.Date(!!timestamp_enq))
}


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


#' Computed variables
#' 
#' Add needed variables as you want 
#' @param df A nested dataframe grouped by user
summarise_variable <- function(df, ...){
  
  if(!is.list(df[,2]))
    stop("Error: Dataset is not nested!")
  
  adds_exp_enq <- enquos(..., .named = TRUE)
  nested_data <- names(df)[2]
  
  add_column <- . %>% 
    summarise(!!!adds_exp_enq)
  
  df %>%
    mutate(adds = purrr::map(df[[nested_data]], add_column)) %>%
    unnest(adds)
}


summarise_by_groups <- function(df, group_vars, summary_vars){
  stopifnot(
    is.list(group_vars),
    is.list(summary_vars)
  )
  cal_column <- . %>% 
    summarise(!!!summary_vars)
  add_column <- . %>% 
    mutate(adds = purrr::map(grouped_data, cal_column))
  df %>% 
    mutate(user_data = purrr::map(user_data, ~.x %>% group_by(!!!group_vars) %>% nest(.key = grouped_data))) %>% 
    mutate(vars = purrr::map(user_data, add_column)) %>% 
    unnest(vars) %>% 
    unnest(adds)
}

#' filter 
#' Keep only users that meet certain preconditions
#' @param df A nested dataframe grouped by user 
#' @param filter_exp A certain condition to meet 
filter_var <- function(df, filter_exp){
  filter_exp_enq <- enquo(filter_exp)
  df %>% 
    filter(!!filter_exp_enq)
}

filter_var_by_user <- function(df, ...){
 
  filter_exp_enq <- enquos(...)
  
  to_filter <- . %>% 
    filter(!!!filter_exp_enq)
  
  df %>% 
    mutate(result = purrr::map(user_data, to_filter)) 
}

#' arrange 
#' arrange order by certain variable 
#' @param df A nested dataframe 
#' @param group_var The varaible to be grouped 
arrange_var <- function(df, ...){
  arrange_exp_enq <- enquos(...)
  df %>% 
    arrange(desc(!!!arrange_exp_enq))
}

arrange_var_by_group <- function(df, group_var, ...){
  group_var_enq <- enquo(group_var)
  arrange_exp_enq <- enquos(...)
  df %>% 
    group_by(!!group_var_enq) %>% 
    arrange(desc(!!!arrange_exp_enq)) 
}


#' remove bots 
#' 
#' remove certain percentage of top users to avoid bots 
#' 
#' @param df A dataframe with columns for the user id, counts point per user 
#' @param user Name of column that holds unique identifier for each user
#' @param counts Number of data points per user
#' @param top_user_percent The percentage of top user you want to remove 

remove_top_n <- function(df, user = "u_id", counts = "counts_per_user", top_user_percent){
  top_user_percent_enq <- enquo(top_user_percent)
  
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  if (!rlang::has_name(df, counts)) {
    stop("Counts per user column does not exist")
  }
  
  user <- rlang::sym(user) 
  counts <- rlang::sym(counts)
  
  num_user <- df %>% pull(!!user) %>% dplyr::n_distinct()
  df %>% 
    dplyr::select(!!user, !!counts) %>%
    unique() %>%
    dplyr::slice(round(num_user*!!top_user_percent_enq):n()) %>%
    left_join(., df)
}

#' add variable 
#' 
#' Add variables as you want/needed 
#' @param df A dataframe with columns for the user id, location and timestamp etc. 
#' @param group_var The variable to be grouped 
#' @param mutate_vars The variables you want to add to the dataframe 
add_column <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  df %>% 
    mutate(!!!adds_exp_enq)
}

add_column_by_group <- function(df, group_vars, mutate_vars){
  stopifnot(
    is.list(group_vars),
    is.list(mutate_vars)
  )
  
  df %>% 
    group_by(!!!group_vars) %>% 
    mutate(!!!mutate_vars) %>% 
    ungroup() %>% 
    unnest() %>% 
    group_by(u_id) %>% 
    nest(.key = user_data)
}


#' score of variables 
#' 
#' Add score of each variables 
#' @param df A nested dataframe by user 
calcu_score_by_user <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  
  add_column <- . %>% 
    mutate(!!!adds_exp_enq)
  
  df %>% 
    mutate(adds = purrr::map(user_data, add_column)) %>% 
    unnest(adds) %>% 
    group_by(u_id) %>% 
    nest(.key = user_data)
}


#' extract location 
#' 
#' Extract most likely home location of each user 
#' @param df A nested dataframe by user 
#' @param score_var A scored variable 
extract_home <- function(df, score_var, ...){
  score_var_enq <- enquo(score_var)
  arrange_vars_enq <- enquos(...)
  
  get_loc <- . %>%
    .[!duplicated(.$GEOID), ] %>% 
    dplyr::arrange(desc(!!!arrange_vars_enq)) %>%
    unique() %>%
    top_n(5) %>%
    filter(!!score_var_enq) %>%
    slice(1:2) %>%
    pull(GEOID) %>%
    paste(., collapse = "; ")
  
  df %>%
    mutate(home = purrr::map(user_data, get_loc)) %>%
    select(-user_data) %>%
    unnest(home)
}















