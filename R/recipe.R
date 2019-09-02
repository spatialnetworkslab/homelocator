#' one-button identify home location
#' 
#' Identify home location with chose recipe 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param recipe Different methods to identify home locations
#' @param show_n_home Number of potential homes to be shown
identify_home <- function(df, user = "u_id", timestamp = "created_at", location = "grid_id", recipe = "homelocator", show_n_home = 1){
  user_exp <- rlang::sym(user)
  timestamp_exp <- rlang::sym(timestamp)
  location_exp <- rlang::sym(location)
  
  df_valid <- validate_dataset(df, user = user, timestamp = timestamp, location = location)
  df_nest <- nest_by_sglGp(df_valid, group_var = user) %>% derive_timestamp(., timestamp = timestamp)
  if(recipe == "homelocator"){
    cleaned_df_byuser <- df_nest %>%
      summarise_var(n_tweets = n(),
                    n_locs = n_distinct(!!location_exp)) %>%
      remove_bots(user = user, counts = "n_tweets", top_u_percent = 0.01) %>%
      filter_var(n_tweets > 10 & n_locs > 10)
    
    cleaned_df_byloc <- cleaned_df_byuser %>%
      summarise_groupVar(vars(!!location_exp),
                         vars(n_tweets_loc = n(),
                              n_hours_loc = n_distinct(hour),
                              n_days_loc = n_distinct(ymd),
                              period_loc = as.numeric(max(!!timestamp_exp) - min(!!timestamp_exp), "days"))) %>%
      filter_in_nest(n_tweets_loc > 10 & n_hours_loc > 10 & n_days_loc > 10 & period_loc > 10)
    
    df_expanded <- cleaned_df_byloc %>%
      add_col_in_nest(wd_or_wk = if_else(wday %in% c(1,7), "weekend", "weekday")) %>%
      add_col_in_nest(numT = lubridate::hour(!!timestamp_exp) + lubridate::minute(!!timestamp_exp) / 60 + lubridate::second(!!timestamp_exp) / 3600) %>%
      add_col_in_nest(rest_or_work = if_else(numT >= 9 & numT <= 18, "work", "rest")) %>%
      add_col_in_nest(wkmorning_or_not = if_else(numT >= 6 & numT <= 12 & wd_or_wk == "weekend", "wkmorning", "not_wkmorning")) %>% 
      summarise_var(n_wdays_loc = n_distinct(wday),
                    n_months_loc = n_distinct(month)) %>%
      add_var_pct(wd_or_wk) %>%
      add_var_pct(rest_or_work) %>%
      add_var_pct(wkmorning_or_not) %>% 
      replace(., is.na(.), 0)
    
    df_score <- df_expanded  %>%
      score_var(group_var = !!user_exp,
                keep_original_vars = F,
                s_rest = 0.2 * (rest),
                s_weekend = 0.1 * (weekend),
                s_wkmorning = 0.1 * (wkmorning),
                s_n_tweets_loc = 0.1 * (n_tweets_loc/max(n_tweets_loc)),
                s_n_days_loc = 0.1 * (n_days_loc/max(n_days_loc)),
                s_period_loc = 0.1 * (period_loc/max(period_loc)),
                s_n_wdays_loc = 0.1 * (n_wdays_loc/7),
                s_n_months_loc = 0.1 * (n_months_loc/12),
                s_n_hours_loc = 0.1 * (n_hours_loc/24)) %>%
      sum_score(user = user, location = location,
                s_rest, s_weekend, s_wkmorning, s_n_tweets_loc, s_n_days_loc, s_period_loc, s_n_wdays_loc, s_n_months_loc, s_n_hours_loc)
    df_score %>% extract_home(score, keep_score = F, show_n_home = show_n_home)
  } else if(recipe == "OSN"){#online social network
    df_moved_bots <- df_nest %>%
      summarise_var(n_tweets = n(),
                    n_locs = n_distinct(!!location_exp)) %>%
      remove_bots(user = user, counts = "n_tweets", top_u_percent = 0.01) 
    
    df_filtered <- df_moved_bots %>%
      filter_var(n_locs > 3) %>%
      filter_in_nest(!wday %in% c(1, 7))  
    
    df_timeframe <- df_filtered %>%
      nest_by_sglGp(., group_var = user) %>%
      add_col_in_nest(tf = if_else(hour >= 2 & hour < 8, "RT", if_else(hour >= 8 & hour < 19, "AT", "LT"))) %>%
      filter_in_nest(tf != "AT") 
    
    
    df_score <- df_timeframe %>%
      nest_by_sglGp(., group_var = user) %>%
      summarise_groupVar(vars(!!location_exp, ymd, tf),
                         vars(n_tweets_tf = n())) %>%
      spread2_in_nest(key = tf, value = n_tweets_tf) %>%
      add_col_in_nest(w_counts = mean(0.744, 0.735, 0.737) * RT + mean(0.362, 0.357, 0.354) * LT) %>%
      summarise_groupVar(vars(!!location_exp),
                         vars(score = sum(w_counts))) 
      
    extract_home(df_score, score, keep_score = F, show_n_home = show_n_home)
  } else if(recipe == "MPD"){ #mobile positioning data
    regular_cells <- df_nest %>% 
      summarise_groupVar(vars(!!location_exp, year, month),
                         vars(n_days_month_loc = n_distinct(ymd))) %>% 
      filter_in_nest(n_days_month_loc >= 2) %>% # at least two tweets at different days a month 
      unnest() 
    regular_nest <- nest_by_sglGp(regular_cells, group_var = "u_id")
    
    regular_append <- regular_nest %>% 
      summarise_groupVar(vars(!!location_exp),
                         vars(n_tweets_loc = n(),
                         n_days_loc = n_distinct(ymd)))
    
    most5popular_regulars <- regular_append %>% 
      arrange_in_nest(group_var = "grid_id", n_days_loc, n_tweets_loc) %>% 
      top_n_in_nest(., n = 3, wt = n_tweets_loc)
    
    regular_filtered <- most5popular_regulars %>% 
      unnest() %>% 
      filter_in_nest(n_days_month_loc >= 7) %>% # remove respondents who have made tweets less than 7 days a month in 2 most frequently visited network cells 
      nest_by_sglGp(., group_var = user) %>% 
      summarise_groupVar(vars(year, month),
                         vars(n_tweets_month = n())) %>% 
      filter_in_nest(n_tweets_month <= 500) %>% # remove respondents who have made more than 500 tweets a month in 2 most frequently visited network cells 
      unnest()
    
    time_line <- chron::times("17:00:00") %>% as.numeric()
    regular_score <- regular_filtered %>% 
      add_col(time = format(!!timestamp_exp, format="%H:%M:%S") %>% chron::times() %>% as.numeric()) %>% 
      nest_by_mulGps(vars(!!user_exp, !!location_exp)) %>% 
      summarise_var(avg_time = mean(time),
                    sd_time = sd(time)) %>% 
      add_col(s_avg_time = if_else(avg_time > time_line, 1, 0)) %>% 
      add_col(s_sd_time = if_else(sd_time > 0.175, 1, 0)) %>% 
      add_col(score = rowSums(.[, c("s_avg_time", "s_sd_time")])) 
    
    regular_score %>% 
      filter_var(score >= 1) %>% 
      group_by(!!user_exp) %>% 
      slice(1:show_n_home) %>% 
      summarise(home =  paste(!!location_exp, collapse = "; "))
  } else if(recipe == "simple"){
    cleaned_df_byuser <- df_nest %>%
      summarise_var(n_tweets = n(),
                    n_locs = n_distinct(!!location_exp)) %>%
      remove_bots(user = user, counts = "n_tweets", top_u_percent = 0.01) %>%
      filter_var(n_tweets > 10 & n_locs > 10)
    
    cleaned_df_byloc <- cleaned_df_byuser %>% 
      summarise_groupVar(vars(!!location_exp),
                         vars(n_tweets_loc = n())) %>% 
      filter_in_nest(n_tweets_loc > 10)
    
    cleaned_df_byloc %>% 
      group_by(!!user_exp, !!location_exp) %>% 
      arrange(desc(n_tweets_loc)) %>% 
      group_by(!!user_exp) %>% 
      slice(1:show_n_home) %>% 
      summarise(home =  paste(!!location_exp, collapse = "; "))
  }
}



