#' Calculate hl_week variable.
#' 
#' 
#' Calculate the percentage of weekday and weekend of each location, throw away location that only has tweets sent on 
#' weekday and keep and store weekend percent value 
#' 
#' @param data A dataframe with columns for the id, timestamp and new added variable columns from var_expand function
calcu_week <- function(data){
  data <- data %>% 
              select(c(hl_week)) %>% 
              group_by(hl_week) %>% 
              summarise(hl_week_counts = n()) %>% 
              mutate(hl_percent_week = hl_week_counts/sum(hl_week_counts),
                     hl_week_type = n_distinct(hl_week)) %>% 
              filter(hl_week_type == 2 | (hl_week_type == 1 & hl_week == 1)) %>% # remove tract only have weekday
              filter(hl_week == 1) %>% ## only keep weekend percent value 
              select(hl_percent_week) 
  
  if (plyr::empty(data)) {
    data <- tibble::tibble(hl_percent_week = c(0))
    data
  } else {
    data
  }
}


#' Calculate Saturday Morning variable
#' 
#' 
#' Calculate the percentage of tweets sent on Saturday morning of the location
#' @inheritParams calcu_week
calcu_sat_morning <- function(data){
  data <- data %>% 
            select(hl_day_of_week, hl_morning_time) %>% 
            filter(hl_morning_time == 1) %>% 
            group_by(hl_day_of_week) %>% 
            summarise(hl_day_of_week_counts = n()) %>% 
            mutate(hl_percent_satmorning = hl_day_of_week_counts/sum(hl_day_of_week_counts)) %>%
            filter(hl_day_of_week == 7) %>% 
            select(hl_percent_satmorning) 
  
  if (plyr::empty(data)) {
    data <- tibble::tibble(hl_percent_satmorning = c(0))
    data
  } else {
    data
  }
}


#' Calculate daytime variable.
#' 
#' 
#' Calculate the percentage of tweets sent on work time and night time of each tract, throw away tracts that percentage 
#' of work time is larger than that of night time, keep and store the percentage of night time
#' @inheritParams calcu_week
calcu_daytimes <- function(data){
    data <- data %>% 
                select(hl_daytimes) %>%
                group_by(hl_daytimes) %>% 
                summarise(hl_daytimes_counts = n()) %>% 
                mutate(hl_percent_daytimes = hl_daytimes_counts/sum(hl_daytimes_counts)) %>% 
                filter(hl_daytimes == 1 & hl_percent_daytimes >= 0.5) %>% #tweets sent during night time is more than that during work time
                select(hl_percent_daytimes)
    
    if (plyr::empty(data)) {
      data <- tibble::tibble(hl_percent_daytimes = c(0))
      data
    } else {
      data
    }
    
}



#' Calculate day variable.
#' 
#' 
#' Calculate unique day of a week of the location
#' @inheritParams calcu_week
calcu_day <- function(data) {
    data %>% 
        select(hl_day_of_week) %>% 
        summarise(hl_unique_dayofweek = n_distinct(hl_day_of_week))
}



#' Calculate month variable.
#' 
#' 
#' Calculate unique month of a year of each tract, and store the result in a list.
#' @inheritParams calcu_week
calcu_month <- function(data) {
    data %>% 
      select(hl_month) %>% 
      summarise(hl_unique_months = n_distinct(hl_month))
}


#' Combine variables for each user of each location
#' 
#' 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location

homeloc_valuate <- function(df, user = "u_id", timestamp = "created_at", location = "GEOID"){
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  if (!rlang::has_name(df, location)) {
    stop("Location column does not exist")
  }
  if (!rlang::has_name(df, timestamp)) {
    stop("Timestamp column does not exist")
  }
  
  if (!is(df %>% pull(!!timestamp), "POSIXct")) {
    stop("Timestamp is not of class POSIXct")
  }
  
  user <- rlang::sym(user) 
  location <- rlang::sym(location)
  timestamp <- rlang::sym(timestamp)
  
  initial_columns <- names(df)
  print("Start expanding new variables ...")
  
  df <- df %>%
    mutate(hl_year = lubridate::year(!!timestamp),
      hl_month = lubridate::month(!!timestamp),
      hl_day = lubridate::day(!!timestamp),
      hl_day_of_week = lubridate::wday(!!timestamp), # Sun is 1 and Sat is 7 
      hl_hour_of_day = lubridate::hour(!!timestamp),
      hl_week = if_else(hl_day_of_week %in% c(1,7), 1, 2),#1 for weekend, 2 for weekday
      hl_times_numeric = lubridate::hour(!!timestamp) + lubridate::minute(!!timestamp) / 60 + lubridate::second(!!timestamp) / 3600,
      hl_daytimes = if_else(hl_times_numeric >= 9 & hl_times_numeric <= 18, 2, 1), # 1 for rest time, 2 for work time
      hl_morning_time = if_else(hl_times_numeric >= 6 & hl_times_numeric <= 12, 1, 2)) %>%# 1 for morning, 2 for afternoon and night
    select(-hl_times_numeric)
  
  after_expanded_columns <- names(df)
  add_columns <- paste(dplyr::setdiff(after_expanded_columns, initial_columns), collapse = ", ")
  print(paste("Finished expanding new variables and the added new variables are:", add_columns))
  
  
  print("Start valuating variables ...")
  df %>% 
    group_by(!!user, !!location, hl_count_location, hl_uniq_hours, hl_uniq_days, hl_period_length) %>% 
    tidyr::nest() %>% 
    mutate( 
      var_1 = furrr::future_map(data, calcu_week),
      var_2 = furrr::future_map(data, calcu_sat_morning),
      var_3 = furrr::future_map(data, calcu_daytimes),
      var_4 = furrr::future_map(data, calcu_day),
      var_5 = furrr::future_map(data, calcu_month)) %>% 
    ungroup() %>% 
    select(-data) %>% 
    tidyr::unnest()
}