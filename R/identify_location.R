#' Identify home locations for users with built-in recipes
#' 
#' This function infers the most possible home locations for users with built-in recipes. 
#' The "recipe" can be either embeded "recipe" - HMLC, OSNA, APDM, FREQ, or self created "recipe". 
#' 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param recipe  Embeded algorithms to identify the most possible home locations for users           
#' @param show_n_loc Number of potential homes to extract
#' @param keep_score Option to keep or remove calculated result/score per user per location
#' @param use_default_threshold Option to use default threshold or customized threshold
#' @param rm_topNpct_user Option to remove or keep the top N percent active users
#' 
#' @importFrom rlang sym
#' @importFrom rlang has_name
#' @importFrom emo ji
#' @importFrom tictoc tic
#' @importFrom tictoc toc
#' 
#' @export
identify_location <- function(df, user = "u_id", timestamp = "created_at", location = "loc_id", recipe, 
                              show_n_loc = 1, keep_score = F, use_default_threshold = T, rm_topNpct_user = F){
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  tictoc::tic("Location Identification")
  ## Validate the input dataset 
  df_valided <- validate_dataset(df, user = user, timestamp = timestamp, location = location)
  
  # Nest the dataset to each user
  df_nested <- nest_verbose(df_valided, c({{location_expr}}, {{timestamp_expr}}))

  ## Derive new variables from timestamp
  df_enriched <- enrich_timestamp(df_nested, timestamp = timestamp)
  
  ## recipe: HMLC
  if(recipe == "HMLC"){
    output <- recipe_HMLC(df_enriched, user = user, timestamp = timestamp, location = location, 
                          show_n_loc, keep_original_vars = F, keep_score = keep_score, 
                          use_default_threshold = use_default_threshold, rm_topNpct_user = rm_topNpct_user)
  } 
  
  ## recipe: FREQ
  if(recipe == "FREQ"){
    output <- recipe_FREQ(df_enriched, user = user, timestamp = timestamp, location = location, show_n_loc, 
                          keep_score = keep_score, use_default_threshold = use_default_threshold, rm_topNpct_user = rm_topNpct_user)
  }
  
  ## recipe: OSNA
  if(recipe == "OSNA"){
    output <- recipe_OSNA(df_enriched, user = user, timestamp = timestamp, location = location, show_n_loc = show_n_loc, 
                          keep_score = keep_score, use_default_threshold = use_default_threshold, rm_topNpct_user = rm_topNpct_user)
  }
  
  ## recipe: APDM
  if(recipe == "APDM"){
    message(paste(emo::ji("exclamation"), "Please make sure you have loaded the neighbors table before you use APDM recipe.\nThe table should have one column named", location,
          "and another column named neighbor.\n The neighbor column should be a list-column contains the neighboring locations for", location, "per row."))
    output <- recipe_APDM(df_enriched, df_neighbors, user = user, timestamp = timestamp, location = location, 
                          keep_score = keep_score, use_default_threshold = use_default_threshold)
  }
  tictoc::toc()
  return(output)
}

#' recipe: homelocator - HMLC
#' @param df An enriched dataframe
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param show_n_loc Number of potential homes to extract
#' @param keep_score Option to keep or remove calculated result/score per user per location
#' @param  keep_original_vars Option to keep or remove columns other than 'user, timestamp, and location'
#' @param use_default_threshold Option to use default threshold or customized threshold
#' @param rm_topNpct_user Option to remove or keep the top N percent active users
#' 
#' @importFrom rlang sym
#' @importFrom lubridate hour
#' @importFrom lubridate minute
#' @importFrom lubridate second
#' 
recipe_HMLC <- function (df, user = "u_id", timestamp = "created_at", location = "loc_id", show_n_loc, 
                         keep_original_vars = F, keep_score = F, use_default_threshold = T, rm_topNpct_user = F) {
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  # use_default_threshold <- readline(prompt = "Do you want to use the default thresholds? (Yes/No): ")
  if(use_default_threshold){
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
    threshold_n_points <- readline(prompt = "What is the minimum number of data points should a user sent in total(default = 10)? Your answer: ") %>% as.integer()
    threshold_n_locs <- readline(prompt = "What is the minimum number of unique places should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_points_loc <- readline(prompt = "What is the minimum number of data points should a user sent at each place (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_hours_loc <- readline(prompt = "What is the minimum number of unique hours should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_n_days_loc <- readline(prompt = "What is the minimum number of days should data points be collected from (default = 10)? Your answer: ") %>% as.integer()
    threshold_period_loc <- readline(prompt = "What is the minimum period should a user being active on the digital platform (default = 10)? Your answer: ") %>% as.integer()
    
    w_n_points_loc <- readline(prompt = "Give a weight to the number of data points at a place (default = 0.1): ") %>% as.integer()
    w_n_hours_loc <- readline(prompt = "Give a weight to the number of unique hours data points were sent a place (default = 0.1): ") %>% as.integer()
    w_n_days_loc <- readline(prompt = "Give a weight to the number of days data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_n_wdays_loc <- readline(prompt = "Give a weight to the number of unique weekdays data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_n_months_loc <- readline(prompt = "Give a weight to the number of unique months data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_period_loc <- readline(prompt = "Give a weight to the period data points were sent at a place (default = 0.1): ") %>% as.integer()
    w_weekend <- readline(prompt = "Give a weight to the proportion of data points sent during weekend at a place (default = 0.1): ") %>% as.integer()
    w_rest <- readline(prompt = "Give a weight the proportion of data points sent during rest time at a place (default = 0.2): ") %>% as.integer()
    w_weekend_am <- readline(prompt = "Give a weight the proportion of data points sent during weekend morning at a place (default = 0.1): ") %>% as.integer()
  }
  
  # users level 
  ## pre-condition set on users 
  df_match_user_condition <- df %>%
    summarise_nested(., n_points = n(),
                     n_locs = n_distinct({{location_expr}})) %>%
    remove_top_users(., user = user, counts = "n_points", topNpct_user = topNpct, rm_topNpct_user = rm_topNpct_user) %>% # remove top N percent active users based on frequency
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


#' recipe: frequency - FREQ
#' 
#' @param df An enriched dataframe
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param show_n_loc Number of potential homes to extract
#' @param keep_score Option to keep or remove calculated result/score per user per location
#' @param use_default_threshold Option to use default threshold or customized threshold
#' @param rm_topNpct_user Option to remove or keep the top N percent active users
#' 
#' @importFrom rlang sym
recipe_FREQ <- function(df, user = "u_id", timestamp = "created_at", location = "loc_id", show_n_loc, 
                        keep_score = F, use_default_threshold = T, rm_topNpct_user = F){
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  # use_default_threshold <- readline(prompt = "Do you want to use the default thresholds? (Yes/No): ")
  if(use_default_threshold){
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
    remove_top_users(., user = user, counts = "n_points", topNpct_user = topNpct, rm_topNpct_user = rm_topNpct_user) %>% # remove top N percent active users based on frequency
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


#' recipe: Online Social Networks Activity - OSNA
#' 
#' 
#' @param df An enriched dataframe
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param show_n_loc Number of potential homes to extract
#' @param keep_score Option to keep or remove calculated result/score per user per location
#' @param use_default_threshold Option to use default threshold or customized threshold
#' @param rm_topNpct_user Option to remove or keep the top N percent active users
#' 
#' @importFrom rlang sym
recipe_OSNA <- function(df, user = "u_id", timestamp = "created_at", location = "loc_id", show_n_loc, 
                        keep_score = F, use_default_threshold = T, rm_topNpct_user = F){
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  
  # use_default_threshold <- readline(prompt = "Do you want to use the default thresholds? (Yes/No): ")
  if(use_default_threshold){
    topNpct <- 1
    threshold_n_locs <- 3
  } else{
    topNpct <- readline(prompt="How many percentage of top active users to be removed (default = 1)? Your answer: ") %>% as.integer()
    threshold_n_locs <- readline(prompt="What is the minumn number of unique places should data points be collected from (default = 3)? Your answer: ") %>% as.integer()
  }
  # users level 
  ## pre-condition set on users 
  df_match_user_condition <- df %>%
    summarise_nested(., 
                     n_points = n(),
                     n_locs = n_distinct({{location_expr}})) %>% 
    remove_top_users(., user = user, counts = "n_points", topNpct_user = topNpct, rm_topNpct_user = rm_topNpct_user) %>% # remove top N percent active users based on frequency
    filter_verbose(., user = user, n_locs > threshold_n_locs) %>% # remove users with data at less than N places 
    filter_nested(., user = user, !wday %in% c(1, 7)) %>%  # remove data sent on weekend, 1 for Sunday and 7 for Saturday 
    mutate_nested(timeframe = if_else(hour >= 2 & hour < 8, "Rest", if_else(hour >= 8 & hour < 19, "Active", "Leisure"))) %>% # add time frame column 
    filter_nested(., user = user, timeframe != "Active") # home location is focused on Rest and Leisure time frame
  
  ## calculate weighted score of data points sent during Leisure and Rest time in different day at different places 
  colnames_nested_data <- df_match_user_condition$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data %in% c(location, "ymd", "timeframe"))] # per location per day per timeframe 
  
  df_timeframe_nested <- df_match_user_condition %>%
    summarise_double_nested(., nest_cols = colnmaes_to_nest, 
                            n_points_timeframe = n()) %>%  # number of data points at the timeframe 
    spread_nested(., key_var = "timeframe", value_var = "n_points_timeframe") %>% # spread the timeframe to columns 
    unnest_verbose() %>% # unnest the result, missed column will be automatically added with NA value
    replace(., is.na(.), 0)  # replace NA with 0
  
  # give the weigh to Rest and Leisure time 
  weight_rest <- mean(0.744, 0.735, 0.737)
  weight_leisure <- mean(0.362, 0.357, 0.354)
  
  df_weighted <- df_timeframe_nested %>% 
    mutate_verbose(score_ymd_loc = weight_rest * Rest + weight_leisure * Leisure) 
  
  # nest by user 
  df_user_nested <- df_weighted %>% nest_verbose(-u_id) 
  
  # calculate the score per location 
  colnames_nested_data <- df_user_nested$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data == location)] # nest by location 
  
  df_scored <- df_user_nested %>% 
    summarise_double_nested(., nest_cols = colnmaes_to_nest, 
                            score = sum(score_ymd_loc))
  
  # extract location based on score value 
  extract_location(df_scored, user = user, location = location, show_n_loc = show_n_loc, keep_score = keep_score, score)
}



#' recipe: Anchor Point Determining Model - APDM
#' 
#' @param df An enriched dataframe
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param show_n_loc Number of potential homes to extract
#' @param keep_score Option to keep or remove calculated result/score per user per location
#' @param use_default_threshold Option to use default threshold or customized threshold
#' 
#' @importFrom rlang sym
#' @importFrom rlang has_name
#' @importFrom emo ji
#' @importFrom chron times
recipe_APDM <- function(df, df_neighbors, user = "u_id", timestamp = "created_at", location = "loc_id", keep_score = F, use_default_threshold = T){
  user_expr <- rlang::sym(user)
  timestamp_expr <- rlang::sym(timestamp)
  location_expr <- rlang::sym(location)
  ## need to check neighbors dataframe, make sure there are columns of locaiton and neighbor, the name of location column should be the same as in the dataset
  if (!rlang::has_name(df_neighbors, location)) {
    stop(paste(emo::ji("bomb"), "Location column does not exist!"))
  }
  
  if (!rlang::has_name(df_neighbors, "neighbor")) {
    stop(paste(emo::ji("bomb"), "Neighbor column does not exist!"))
  }
  # use_default_threshold <- readline(prompt = "Do you want to use the default thresholds? (Yes/No): ")
  if(use_default_threshold){
    threshold_n_days_per_month_loc <- 7
    threshold_n_points_per_month_loc <- 500
  } else{
    threshold_n_days_per_month_loc <- readline(prompt="What is the minium active days per month (default = 7)? Your answer: ") %>% as.integer()
    threshold_n_points_per_month_loc <- readline(prompt="What is the maximum data points sent per month (default = 500)? Your answer: ") %>% as.integer()
  }
  ## Removal of places with too high or too low a number of data points made
  ## respondents who have data points at their top n frequently visited places fewer than 7 days a month will be removed from the database
  ## respondents are considered to have too many data points if they have more than 500 data points a month in their frequently visited places
  colnames_nested_data <- df$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data %in% c(location, "year", "month"))]
  df_cleaned <- df %>%
    summarise_double_nested(., nest_cols = colnmaes_to_nest,
                            n_days_per_month_loc = n_distinct(day),
                            n_points_per_month_loc = n()) %>%
    filter_nested(., user = user,
                  n_days_per_month_loc >= threshold_n_days_per_month_loc & n_points_per_month_loc <= threshold_n_points_per_month_loc)
  
  # get the top 5 most frequently visited location
  df_user_nested <- df_cleaned %>% unnest_nested(., data)
  colnames_nested_data <- df_user_nested$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data == location)]
  df_top5_locs <- df_user_nested %>%
    summarise_double_nested(., nest_cols = colnmaes_to_nest,
                            n_days_loc = n_distinct(ymd),
                            n_points_loc = n()) %>%
    arrange_nested(., n_days_loc, n_points_loc) %>%
    top_n_nested(., n = 5, wt = "n_points_loc") %>% 
    unnest_verbose()
  
  # extract the start data point per day for each location 
  colnames_nested_data <- df_top5_locs$data[[1]] %>% names()
  colnmaes_to_nest <- colnames_nested_data[-which(colnames_nested_data %in% c("ymd"))] # nest to per user per location per day
  df_with_start_point <- df_top5_locs %>%
    summarise_double_nested(., nest_cols = colnmaes_to_nest,
                            start_point = min({{timestamp_expr}})) # get the start data point of a day 
  
  # add home or work label to top 5 most frequently visit places: 1 means home and 0 means work
  time_line <- chron::times("17:00:00") %>% as.numeric() 
  df_with_home_type <- df_with_start_point %>% 
    mutate_nested(start_time = format(start_point, format = "%H:%M:%S") %>% chron::times() %>% as.numeric()) %>% 
    summarise_nested(mean_start_time = mean(start_time), 
                     sd_start_time = sd(start_time)) %>% 
    mutate_verbose(location_type = if_else(mean_start_time < time_line & sd_start_time <= 0.175, 0, 1)) %>% # 1 means home and 0 means work
    filter(location_type != 0) %>% # remove work locations
    nest_verbose(-{{user_expr}}) %>% 
    summarise_nested(n_home = n()) # get number of returned home locations
  
  ## for one home users, directly extract the location as their homes 
  df_one_home <- df_with_home_type %>% filter(n_home == 1)
  
  output <- extract_location(df_one_home %>% dplyr::select(-n_home), user = user, location = location, show_n_loc = 1, keep_score = keep_score, n_days_loc, n_points_loc)
  
  ## for multiple home users, take the top 2 most frequently visit places and do the comparision
  df_multiple_homes <- df_with_home_type %>% 
    filter(n_home > 1) %>% 
    arrange_nested(., desc(n_days_loc), desc(n_points_loc)) # order the location by number of days and number of data points in descending order
  
  
  decide_home <- function(data){
    homes <- data %>% pull({{location_expr}})
    home1 <- homes[1]
    home1_neighbors <- df_neighbors %>% filter({{location_expr}} == home1) %>% pull(neighbor) %>% unlist() # get the neighboring locations of first home 
    home2 <- homes[2]
    # if home 1 and home 2 are neighboring location, extract home1 (one with the higher number of days, and the higher number of data points)
    if(home2 %in% home1_neighbors){
      return(home1)
    } else{
      n_days_homes <- data %>% pull(n_days_loc)
      n_days_home1 <- n_days_homes[1]
      n_days_home2 <- n_days_homes[2]
      # if the most frequently visited location covers more than 75 percent of the days a user stayed at the two most frequently visited locations, return the place as home
      if((n_days_home1/(n_days_home1 + n_days_home2)) > 0.75){
        return(home1)
      } else{
        sd_homes <- data %>% pull(sd_start_time)
        sd_home1 <- sd_homes[1]
        sd_home2 <- sd_homes[2]
        # the location with the larger standard deviation is classified as the home 
        if(sd_home1 > sd_home2){
          return(home1)
        } else{
          return(home2)
        }
      }
    }
  }
  
  if(keep_score){
    df_multiple_homes %>% 
      dplyr::select(-n_home) %>% 
      mutate_verbose(., home = map_chr(data, function(x) decide_home(x))) %>% 
      bind_rows(., output)
  } else{
    df_multiple_homes %>% 
      mutate_verbose(., home = map_chr(data, function(x) decide_home(x))) %>% 
      dplyr::select(u_id, home) %>% 
      bind_rows(., output)
  }
  
}
