#' Weight and calculate score of variable 
#' 
#' 
#' Give a 0-1 range weight and score to each variable, and add up them to get a final score for each tract, order the tracts 
#' according to the score. 
#' 
#' @param data A dataframe with columns for location, hl_count_location, hl_uniq_hours, hl_uniq_days, hl_period_length, hl_percent_week, hl_percent_satmorning, 
#' hl_percent_daytimes, hl_unique_dayofweek and hl_unique_months
#' @param location Name of column that holds unique identifier for each location
#' @param w_count_location Weight of tweets counts sent at a location variable               
#' @param w_uniq_hours Weight of unique hours variable
#' @param w_uniq_days Weight of unique days variable
#' @param w_period_length Weight of study period length variable
#' @param w_percent_week Weight of percentage of tweets sent on weekend variable 
#' @param w_percent_satmorning Weight of percentage of tweets sent on Saturday morning variable 
#' @param w_percent_daytimes Weight of percentage of tweets sent at night time variable 
#' @param w_uniq_dayofweek Weight of unique day of week variable 
#' @param w_uniq_month Weight of unique month variable 

calcu_scores <- function(data, location = "GEOID", w_count_location = 0.1, w_uniq_hours = 0.1, w_uniq_days = 0.05, w_period_length =  0.05, w_percent_week = 0.2,
                                                   w_percent_satmorning = 0.2, w_percent_daytimes = 0.2, w_uniq_dayofweek = 0.05, w_uniq_month = 0.05){
  if (!rlang::has_name(df, location)) {
    stop("Location column does not exist")
  }
  location <- rlang::sym(location)

  data %>%
    transmute(GEOID = as.character(!!location),
      hl_score_count_location = !!w_count_location * (hl_count_location/max(hl_count_location)),
      hl_score_uniq_hours = !!w_uniq_hours * (hl_uniq_hours/24),
      hl_score_uniq_days = !!w_uniq_days * (hl_uniq_days/max(hl_uniq_days)),
      hl_score_period_length = !!w_period_length * (as.numeric(hl_period_length)/max(as.numeric(hl_period_length))),
      hl_score_percent_week = !!w_percent_week * hl_percent_week,
      hl_score_percent_satmorning = !!w_percent_satmorning * hl_percent_satmorning,
      hl_score_percent_daytimes = !!w_percent_daytimes * hl_percent_daytimes,
      hl_score_unique_dayofweek = !!w_uniq_dayofweek * (hl_unique_dayofweek/7),
      hl_score_unique_months = !!w_uniq_month * (hl_unique_months/12)) %>%
    transform(., hl_score = rowSums(.[ , -1]) )
}

#' Add score to users
#' 
#' 
#' @param df A dataframe with columns for the user id, location, timestamp
homeloc_score <- function(df,  user = "u_id") {
  if (!rlang::has_name(df, user)) {
    stop("User column does not exist")
  }
  user <- rlang::sym(user) 
  print(paste("Start scoring locations ..."))
  
  df %>% 
    group_by(!!user) %>% 
    tidyr::nest() %>% 
    mutate(scores = furrr::future_map(data, calcu_scores)) %>% 
    select(-data) %>%
    unnest() %>% 
    ungroup()
}