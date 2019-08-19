#' one-button identify home location
#' 
#' TIdentify home location with chose recipe 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param recipe Different methods to identify home locations
#' 
identify_home <- function(df, user = "u_id", timestamp = "created_at", location = "grid_id", recipe = "homelocator"){
  
  user <- rlang::sym(user) 
  timestamp <- rlang::sym(timestamp)
  location <- rlang::sym(location)
  recipe <- rlang::sym(recipe)
  name_user <- quo_name(user)
  name_timestampe <- quo_name(timestamp)
  name_location <- quo_name(location)
  name_recipe <- quo_name(recipe)
  
  df <- validate_dataset(df, user = name_user, timestamp = name_timestampe, location = name_location, keep_other_vars = F)
  df_nest <- nest_by_sglGp(df, group_var = !!user) %>%
    derive_timestamp(., timestamp = !!timestamp)
  if(recipe == "homelocator"){
    df_filtered <- df_nest %>%
      summarise_var(n_tweets = n(),
                    n_locs = n_distinct(!!location)) %>%
      remove_bots(user = name_user, counts = "n_tweets", top_u_percent = 0.01) %>%
      filter_var(n_tweets > 10 & n_locs > 10) %>%
      summarise_groupVar(vars(!!location),
        vars(n_tweets_loc = n(),
             n_hours_loc = n_distinct(hour),
             n_days_loc = n_distinct(ymd),
             period_loc = as.numeric(max(!!timestamp) - min(!!timestamp), "days"))) %>%
      filter_in_nest(n_tweets_loc > 5 & n_hours_loc > 5 & n_days_loc > 5 & period_loc > 5)
    
    
    df_expanded <- df_filtered %>%
      add_col_in_nest(wd_or_wk = if_else(wday %in% c(1,7), "weekend", "weekday")) %>%
      add_col_in_nest(numT = lubridate::hour(!!timestamp) + lubridate::minute(!!timestamp) / 60 + lubridate::second(!!timestamp) / 3600) %>%
      add_col_in_nest(rest_or_work = if_else(numT >= 9 & numT <= 18, "work", "rest")) %>%
      add_col_in_nest(early_or_late = if_else(numT >= 6 & numT <= 12, "morning", "noon_night"))
    
    df_score <- df_expanded %>%
      summarise_var(n_wdays_loc = n_distinct(wday),
                    n_months_loc = n_distinct(month)) %>%
      add_var_pct(wd_or_wk) %>%
      add_var_pct(rest_or_work) %>%
      add_var_pct(early_or_late) %>%
      filter_var(rest >= 0.5)  %>%
      score_var(group_var = u_id, keep_original_vars = F,
                s_rest = 0.2 * (rest),
                s_weekend = 0.2 * (weekend),
                s_n_tweets_loc = 0.1 * (n_tweets_loc/max(n_tweets_loc)),
                s_n_days_loc = 0.1 * (n_days_loc/max(n_days_loc)),
                s_period_loc = 0.1 * (period_loc/max(period_loc)),
                s_n_wdays_loc = 0.1 * (n_wdays_loc/7),
                s_n_months_loc = 0.1 * (n_months_loc/12),
                s_n_hours_loc = 0.1 * (n_hours_loc/24)) %>%
              sum_score(user = "u_id", location = "grid_id",
                s_rest, s_weekend, s_n_tweets_loc, s_n_days_loc, s_period_loc, s_n_wdays_loc, s_n_months_loc, s_n_hours_loc)
    df_score %>% extract_home(score, keep_score = F)
  }
  
  if(recipe == "OSN"){
    df_nest %>% 
      summarise_var(n_tweets = n(), 
                    n_locs = n_distinct(!!location)) %>% 
      remove_bots(user = name_user, counts = "n_tweets", top_u_percent = 0.01) %>%
      filter_var(n_locs > 3) %>% 
      filter_in_nest(!wday %in% c(1, 7))  %>% 
      nest_by_sglGp(., !!user) %>% 
      add_col_in_nest(tf = if_else(hour >= 2 & hour < 8, "RT", if_else(hour >= 8 & hour < 19, "AT", "LT"))) %>% 
      filter_in_nest(tf != "AT") %>% 
      nest_by_sglGp(., !!user) %>% 
      summarise_groupVar(vars(!!location, ymd, tf), 
                         vars(n_tweets_tf = n())) %>% 
      spread2_in_nest(key = tf, value = n_tweets_tf) %>% 
      add_col_in_nest(w_counts = mean(0.744, 0.735, 0.737) * RT + mean(0.362, 0.357, 0.354) * LT) %>% 
      summarise_groupVar(vars(!!location), 
                         vars(score = sum(w_counts))) %>% 
      extract_home(score, keep_score = F)
  }
  
}



