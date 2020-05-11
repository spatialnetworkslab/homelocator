#' Identify home locations for users
#' 
#' This function infers the most possible home locations for users with "recipe". 
#' The "recipe" can be either embeded "recipe" - HMLC, OSNA, APDM, FREQ, or self created "recipe". 
#' 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param recipe Different methods to identify home locations
#' @param show_n_loc Number of potential homes to be shown
#' @param rm_pct Percentage of the top active users that to be removed
#' 
#' 
#' @export
identify_location <- function(df, user = "u_id", timestamp = "created_at", location = "loc_id", tz = "Asia/Singapore", recipe, show_n_loc = 1, keep_score = F){
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  ## Validate the input dataset 
  df_valided <- validate_dataset(df, user = user, timestamp = timestamp, location = location)
  
  # Nest the dataset to each user
  df_nested <- nest_verbose(df_valided, c({{location_expr}}, {{timestamp_expr}}))

  ## Derive new variables from timestamp
  df_enriched <- enrich_timestamp(df_nested, timestamp = timestamp, tz = tz)
  
  if(recipe == "HMLC"){
    recipe_HMLC(df_enriched, user = user, timestamp = timestamp, location = location, show_n_loc, keep_original_vars = F, keep_score = keep_score)
  } 
  
  if(recipe == "FREQ"){
    recip_FREQ(df_enriched, user = user, timestamp = timestamp, location = location, show_n_loc, keep_score = keep_score)
  }
}


# recipe: homelocator - HMLC
recipe_HMLC <- function (df, user = "u_id", timestamp = "created_at", location = "loc_id", show_n_loc, keep_original_vars = F, keep_score = F) {
  
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  use_default_threshold <- readline(prompt = "Do you want to use the default thresholds? (Yes/No): ")
  if(use_default_threshold == "Yes"){
    topNpct <- 1
    threshold_n_points <- 10
    threshold_n_locs <- 10
    threshold_n_points_loc <- 10
    threshold_n_hours_loc <- 10
    threshold_n_days_loc <- 10
    threshold_period_loc <- 10
    w_n_points_loc <- 0.1
    w_n_hours_loc <- 0.1
    w_n_days_loc <- 0.1
    w_n_wdays_loc <- 0.1
    w_n_months_loc <- 0.1
    w_period_loc <- 0.1
    w_weekend <- 0.1
    w_rest <- 0.2
    w_weekend_am <- 0.1
  } else{
    topNpct <- readline(prompt="How many percentage of top active users to be removed (default = 1)? Your answer: ") %>% as.integer()
    threshold_n_points <- readline(prompt = "What is the minumn number of data points should a user sent in total(default = 10)? Your answer: ") %>% as.integer()
    threshold_n_locs <- readline(prompt = "What is the minumn number of unique places should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_points_loc <- readline(prompt = "What is the minumn number of data points should a user sent at each place (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_hours_loc <- readline(prompt = "What is the minumn number of unqiue hours should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_days_loc <- readline(prompt = "What is the minum number of days should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_period_loc <- readline(prompt = "What is the minum period should a user being active on the digital platform (default = 10)? Your answer: ") %>% as.integer()
    
    w_n_points_loc <- readline(prompt = "Give a weight to the number of data points at a place (default = 0.1): ") %>% as.integer()
    w_n_hours_loc <- readline(prompt = "Give a weight to the number of unique hours data points were sent a place (default = 0.1): ") %>% as.integer()
    w_n_days_loc <- readline(prompt = "Give a weight to the number of days data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_n_wdays_loc <- readline(prompt = "Give a weight to the number of unique weekdays data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_n_months_loc <- readline(prompt = "Give a weight to the number of unique months data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_period_loc <- readline(prompt = "Give a weight to the period data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_weekend <- readline(prompt = "Give a weight to the proportion of data points sent during weekend at a place (default = 0.1): ") %>% as.integer()
    w_rest <- readline(prompt = "Give a weight the proportion of data points sent during rest time at a place (defaust = 0.2): ") %>% as.integer()
    w_weekend_am <- readline(prompt = "Give a weight the proportion of data points sent during weekend morning at a place (defaust = 0.1): ") %>% as.integer()
  }
  
  # users level 
  ## pre-condition set on users 
  df_match_user_condition <- df %>%
    summarise_nested(., n_points = n(),
                     n_locs = n_distinct({{location_expr}})) %>%
    remove_top_users(., user = user, counts = "n_points", topNpct_user = topNpct) %>% # remove top N percent active users based on frequency
    filter_verbose(., user = user, n_points > threshold_n_points & n_locs > threshold_n_locs) # each use at least has more than threshold_n_points data points sent at threshold_n_locs different locations
  
  # location level 
  # pre-condition set on locations     
  colnames_nested_data <- df_match_user_condition$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data == location)]
  
  df_match_loc_condition <- df_match_user_condition %>%
    summarise_double_nested(., nest_cols = colnmaes_to_nest, # summarise on location level 
                            n_points_loc = n(), 
                            n_hours_loc = n_distinct(hour), 
                            n_days_loc = n_distinct(ymd), 
                            period_loc = as.numeric(max({{timestamp_expr}}) - min({{timestamp_expr}}), "days")) %>% 
    unnest_verbose(data) %>% # one row shows data point of per user per location
    filter_verbose(., user = user, 
                      n_points_loc > threshold_n_points_loc & n_hours_loc > threshold_n_hours_loc & n_days_loc > threshold_n_days_loc & period_loc > threshold_period_loc) %>%  # each use at least has threshold_n_points_loc data points sent at threshold_n_hours_loc different hours for at least threshold_n_days_loc different days at each location, the whole time period is at least more than threshold_period_loc days 
    summarise_nested(n_wdays_loc = n_distinct(wday), # add new variable after matching the conditions
                     n_months_loc = n_distinct(month))                               
                                                                                                       
  # add new variables 
  df_expanded <- df_match_loc_condition %>% 
    mutate_nested(., 
                  wd_or_wk = if_else(wday %in% c(1,7), "weekend", "weekday"), # 1 means Sunday and 7 means Saturday 
                  time_numeric = lubridate::hour({{timestamp_expr}}) + lubridate::minute({{timestamp_expr}})/60 + lubridate::second({{timestamp_expr}})/3600, 
                  rest_or_work = if_else(time_numeric >= 9 & time_numeric <= 18, "work", "rest"), 
                  wk.am_or_wk.pm = if_else(time_numeric >= 6 & time_numeric <= 12 & wd_or_wk == "weekend", "weekend_am", "weekend_pm")) %>% 
    prop_factor_nested(wd_or_wk, rest_or_work, wk.am_or_wk.pm)
    
  ## assign weight to variables and sum variables
  df_scored <- df_expanded  %>%
    score_nested(., user = user, location = location, # score each variables with weight
                 keep_original_vars = keep_original_vars,
                 s_n_points_loc = w_n_points_loc * (n_points_loc/max(n_points_loc)),
                 s_n_hours_loc = w_n_hours_loc * (n_hours_loc/24),
                 s_n_days_loc = w_n_days_loc * (n_days_loc/max(n_days_loc)),
                 s_n_wdays_loc = w_n_wdays_loc * (n_wdays_loc/7),
                 s_n_months_loc = w_n_months_loc * (n_months_loc/12),
                 s_period_loc = w_period_loc * (period_loc/max(period_loc)),
                 s_weekend = w_weekend * (weekend),
                 s_rest = w_rest * (rest),
                 s_weekend_am = w_weekend_am * (weekend_am)) %>% 
    score_summary(., user = user, location = location, starts_with("s_")) # sum all scores for each location
  
  # extract locations based on score value 
  df_scored %>% 
    extract_location(., user = user, location = location, show_n_loc = show_n_loc, keep_score = keep_score, score)
}


# recipe: frequency - FREQ
recip_FREQ <- function(df, user = "u_id", timestamp = "created_at", location = "loc_id", show_n_loc, keep_score = F){
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  use_default_threshold <- readline(prompt = "Do you want to use the default thresholds? (Yes/No): ")
  if(use_default_threshold == "Yes"){
    topNpct <- 1
    threshold_n_points <- 10
    threshold_n_locs <- 10
    threshold_n_points_loc <- 10
  } else{
    topNpct <- readline(prompt="How many percentage of top active users to be removed (default = 1)? Your answer: ") %>% as.integer()
    threshold_n_points <- readline(prompt="What is the minumn number of data points should a user sent in total(default = 10)? Your answer: ") %>% as.integer()
    threshold_n_locs <- readline(prompt="What is the minumn number of unique places should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_points_loc <- readline(prompt="What is the minumn number of data points should a user sent at each place (default = 10)? Your answer: ") %>% as.integer()
  }
  
  # users level 
  ## pre-condition set on users 
  df_match_user_condition <- df %>%
    summarise_nested(., 
                     n_points = n(),
                     n_locs = n_distinct({{location_expr}})) %>%
    remove_top_users(., user = user, counts = "n_points", topNpct_user = topNpct) %>% # remove top N percent active users based on frequency
    filter_verbose(., user = user, n_points > threshold_n_points & n_locs > threshold_n_locs) # each use at least has more than threshold_n_points data points sent at threshold_n_loc different locations
  
  # location level 
  # pre-condition set on locations     
  colnames_nested_data <- df_match_user_condition$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data == location)]
  
  df_match_loc_condition <- df_match_user_condition %>%
    summarise_double_nested(., nest_cols = colnmaes_to_nest, # summarise on location level 
                            n_points_loc = n()) %>% 
    filter_nested(., user = user, n_points_loc > threshold_n_points_loc)  
  
  # extract locations based on frequency of data points sent on locations
  df_match_loc_condition %>% 
    extract_location(., user = user, location = location, show_n_loc = show_n_loc, keep_score = keep_score, n_points_loc)
}


