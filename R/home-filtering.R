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




#' Initial filter.
#' 
#' Keep only users that meet certain preconditions.
#' 
#' 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param min_count_user Min. number of data points per user
#' @param min_count_location Min. number of data points per user, per location
#' @param min_period_length Min. number of days between first and last day user was active at location
#' @param min_days Min. number of unique days user was active at location
#' @param min_hours Min. number of unique hours user was active at location

homeloc_filter <- function(df, user = "u_id", timestamp = "created_at", location = "GEOID", min_count_user = 10,
                           min_count_location = 10, min_period_length = 10, min_days = 10, min_hours = 10, keep_intermediate_vars = F) {
  
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  if (!rlang::has_name(df, timestamp)) {
    stop("Timestamp column does not exist")
  }
  if (!rlang::has_name(df, location)) {
    stop("Location column does not exist")
  }
  
  user <- rlang::sym(user) 
  timestamp <- rlang::sym(timestamp)
  location <- rlang::sym(location)
  
  if (!is(df %>% pull(!!timestamp), "POSIXct")) {
    stop("Timestamp is not of class POSIXct")
  }
  
  unique_users <- df %>% pull(!!user) %>% n_distinct()
  print(paste("Starting with", unique_users, "unique users"))
  
  df <- df %>%
    group_by(!!user) %>% 
    mutate(hl_count_user = n()) %>% 
    filter(hl_count_user > !!min_count_user) %>% 
    group_by(!!location, !!user) %>%
    mutate(hl_count_location = n(),
           hl_uniq_hours = n_distinct(lubridate::hour(!!timestamp)),
           hl_uniq_days = n_distinct(as.Date(!!timestamp)),
           hl_period_length = as.numeric(max(!!timestamp) - min(!!timestamp), "days")) %>% 
    filter(hl_count_location > !!min_count_location,
           hl_uniq_hours > !!min_hours,
           hl_uniq_days > !!min_days,
           hl_period_length > !!min_period_length) %>% 
    ungroup()
  
  unique_users <- df %>% pull(!!user) %>% n_distinct()
  print(paste("After filtering,", unique_users, "unique users remain"))
  
  if (keep_intermediate_vars) {
    df
  } else {
    df %>% 
      select(-count_user, -count_location, -uniq_hours, -uniq_days, -period_length)
  }
}


#' Expand variables to dataset.
#' 
#' 
#' Add "year, month, day, day of week, hour of day, week, daytimes, times and group columnes to dataset. 
#' "year, month, day, day of week, hour of day, week, daytimes, times" are produced from timestamped variable,
#' and group is produced from total tweets counts according to diffrent range.
#' 
#' @param df A dataframe with timestamped variable
filtering <- function(df){
    df %>% as_tibble() %>% select(c(u_id, created_at, GEOID)) %>% 
        group_by(u_id) %>% 
        nest() %>% 
        mutate(data = future_map(data, home_filtering)) %>%
        unnest() %>%
        group_by(u_id) %>%
        mutate(total_counts = n(),
               year = year(date),
               month = month(date),
               day = day(date),
               day_of_week = wday(date), # Sun is 1 and Sat is 7
               hour_of_day = hour(date),
               week = if_else(day_of_week %in% c(1, 7), 1, 2), ## 1 for weekend, 2 for weekday
               times_numeric = hour(date) + minute(date) / 60 + second(date) / 3600,
               daytimes = if_else(times_numeric >= 9 & times_numeric <= 18, 2, 1), # 1 for night time, 2 for work time
               times = if_else(times_numeric >= 6 & times_numeric <= 12, 1, 2), # 1 for morning, 2 for afternoon and night
               group = cut2(total_counts, c(0, 100, 1000, 2000, 3000, 4000, 5000, 10000, 15000, 25000))) %>%
        select(-c(date, times_numeric)) %>% nest()
}




