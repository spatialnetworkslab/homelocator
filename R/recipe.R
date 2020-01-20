#' one-button identify home location
#' 
#' Identify home location with chose recipe 
#' @param df A dataframe with columns for the user id, location, timestamp
#' @param user Name of column that holds unique identifier for each user
#' @param timestamp Name of timestamp column. Should be POSIXct
#' @param location Name of column that holds unique identifier for each location
#' @param recipe Different methods to identify home locations
#' @param show_n_home Number of potential homes to be shown
identify_loc <- function(df, user = "u_id", timestamp = "created_at", location = "grid_id", recipe = "HLC", 
  show_n_loc = 1, rm_pct = 0.01){
  user_exp <- rlang::sym(user)
  timestamp_exp <- rlang::sym(timestamp)
  location_exp <- rlang::sym(location)
  ## Validate the input dataset 
  df_valid <- validate_dataset(df, user = user, timestamp = timestamp, location = location)
  ## Nest the dataset to each user
  df_nest <- nest_cols(df_valid, c({{location_exp}}, {{timestamp_exp}})) 
  ## Derive time related variables from timestamp
  df_enrich <- enrich_timestamp(df_nest, timestamp = timestamp)
  if(recipe == "HLC"){
    #pre-condition set on users 
    cleaned_df_byuser <- df_enrich %>%
      summ_in_nest(n_tweets = n(),
                   n_locs = n_distinct({{location_exp}})) %>%
      remove_bots(user = user, counts = "n_tweets", topNpct_user = rm_pct) %>%
      filter_var(n_tweets > 10 & n_locs > 10)
    
    #pre-condition set on locations     
    data_cols_nm <- cleaned_df_byuser$data[[1]] %>% names()
    nest_cols_nm <- data_cols_nm[-which(data_cols_nm == location)]
    
    cleaned_df_byloc <- cleaned_df_byuser %>%
      grpSumm_in_nest(., nest_cols = nest_cols_nm, 
                      vars(n_tweets_loc = n(),
                           n_hrs_loc = n_distinct(hour),
                           n_days_loc = n_distinct(ymd),
                           period_loc = as.numeric(max({{timestamp_exp}}) - min({{timestamp_exp}}), "days"))) %>% 
      unnest_cols(data) %>% 
      filter_var(n_tweets_loc > 10 & n_hrs_loc > 10 & n_days_loc > 10 & period_loc > 10)
    
    ## create new variables 
    df_expanded <- cleaned_df_byloc %>% 
      add_col_in_nest(wd_or_wk = if_else(wday %in% c(1,7), "weekend", "weekday")) %>%
      add_col_in_nest(numTime = lubridate::hour({{timestamp_exp}}) + lubridate::minute({{timestamp_exp}}) / 60 + lubridate::second({{timestamp_exp}}) / 3600) %>%
      add_col_in_nest(rest_or_work = if_else(numTime >= 9 & numTime <= 18, "work", "rest")) %>%
      add_col_in_nest(wk_am = if_else(numTime >= 6 & numTime <= 12 & wd_or_wk == "weekend", "wk_am", "none_wk_am")) %>% 
      summ_in_nest(n_wdays_loc = n_distinct(wday),
                   n_months_loc = n_distinct(month)) %>% 
      add_pct_in_nest(wd_or_wk) %>%
      add_pct_in_nest(rest_or_work) %>%
      add_pct_in_nest(wk_am) %>%
      replace(., is.na(.), 0)
    ## assign weight to variables and sum variables
    df_score_var <- df_expanded  %>%
      score_var(user = user, location = location, keep_ori_vars = F,
                s_n_tweets_loc = 0.1 * (n_tweets_loc/max(n_tweets_loc)),
                s_n_hrs_loc = 0.1 * (n_hrs_loc/24),
                s_n_days_loc = 0.1 * (n_days_loc/max(n_days_loc)),
                s_period_loc = 0.1 * (period_loc/max(period_loc)),
                s_n_wdays_loc = 0.1 * (n_wdays_loc/7),
                s_n_months_loc = 0.1 * (n_months_loc/12),
                s_weekend = 0.1 * (weekend),
                s_rest = 0.2 * (rest),
                s_wk_am = 0.1 * (wk_am)) 
    
    df_sum_score <- df_score_var %>% 
      sum_score(user = user, location = location, 
                s_rest, s_weekend, s_wk_am, s_n_tweets_loc, s_n_days_loc, s_period_loc, s_n_wdays_loc, s_n_months_loc, s_n_hrs_loc)
    
    inferred_loc <- df_sum_score %>% extract_loc(vars(score), show_n_loc = show_n_loc, keep_score = F)
    inferred_loc

  } else if(recipe == "OSN"){#online social network
    df_moved_bots <- df_enrich %>%
      summ_in_nest(n_tweets = n(),
                   n_locs = n_distinct({{location_exp}})) %>% 
      remove_bots(user = user, counts = "n_tweets", topNpct_user = rm_pct) 
    
    ## pre-conditions
    df_filtered <- df_moved_bots %>%
      filter_var(n_locs > 3) %>%
      filter_in_nest(!wday %in% c(1, 7))  
    
    df_timeframe <- df_filtered %>%
      add_col_in_nest(tf = if_else(hour >= 2 & hour < 8, "RT", if_else(hour >= 8 & hour < 19, "AT", "LT"))) %>%
      filter_in_nest(tf != "AT") 
    
    
    #pre-condition set on locations     
    data_cols_nm <- df_timeframe$data[[1]] %>% names()
    nest_cols_nm <- data_cols_nm[-which(data_cols_nm %in% c(location, "ymd", "tf"))]
    
    df_score <- df_timeframe %>%
      grpSumm_in_nest(nest_cols = nest_cols_nm, vars(n_tweets_tf = n())) %>% 
      # add_col(empty_tb = map_lgl(data, plyr::empty)) %>%
      # filter_var(empty_tb == F) %>%
      spread2_in_nest(key_var = tf, value_var = n_tweets_tf)  %>% 
      spread2_add_missing(c("LT", "RT")) %>% 
      add_col_in_nest(w_counts = mean(0.744, 0.735, 0.737) * RT + mean(0.362, 0.357, 0.354) * LT) 
    
    score_data_cols_nm <- df_score$data[[1]] %>% names()
    to_nest_cols_nm <- score_data_cols_nm[-which(score_data_cols_nm == location)]
    
    df_sum_score <- df_score %>%
      grpSumm_in_nest(nest_cols = to_nest_cols_nm, vars(score = sum(w_counts)))
    
    inferred_loc <- df_sum_score %>% 
      extract_loc(vars(score), show_n_loc = show_n_loc, keep_score = F)
    inferred_loc
  } else if(recipe == "MPD"){ #mobile positioning data

    data_cols_nm <- df_enrich$data[[1]] %>% names()
    nest_cols_nm <- data_cols_nm[-which(data_cols_nm %in% c(location))]
    
    df_appended <- df_enrich %>% 
      grpSumm_in_nest(nest_cols = nest_cols_nm, 
                      vars(n_tweets_loc = n(),
                           n_days_loc = n_distinct(ymd)))
    
    df_top5popular <- df_appended %>% 
      arrange_in_nest(group_var = location, n_days_loc, n_tweets_loc) %>% 
      top_n_in_nest(., n = 5, wt = n_tweets_loc) %>% 
      unnest_cols_in_nest()
    
    time_line <- chron::times("17:00:00") %>% as.numeric()
    top5_data_cols_nm <- df_top5popular$data[[1]] %>% names()
    top5_nest_cols_nm <- top5_data_cols_nm[-which(top5_data_cols_nm %in% c(location, "n_days_loc", "n_tweets_loc", "year", "month"))]
    
    
    df_filtered <- df_top5popular %>% 
      grpSumm_in_nest(nest_cols = top5_nest_cols_nm, 
                      vars(n_days_month_loc = n_distinct(day), 
                      n_tweets_month = n())) %>% 
      filter_in_nest(n_days_month_loc >= 7) %>% # remove respondents who have made tweets less than 7 days a month in 2 most frequently visited network cells 
      filter_in_nest(n_tweets_month <= 500) %>% # remove respondents who have made more than 500 tweets a month in 2 most frequently visited network cells 
      unnest_cols_in_nest() %>% 
      add_col_in_nest(time = format({{timestamp_exp}}, format="%H:%M:%S") %>% chron::times() %>% as.numeric()) 
    
    filter_data_cols_nm <- df_filtered$data[[1]] %>% names()
    filter_nest_cols_nm <- filter_data_cols_nm[-which(filter_data_cols_nm %in% c(location))]
    
    df_score <- df_filtered %>% 
      grpSumm_in_nest(nest_cols = filter_nest_cols_nm, 
                      vars(avg_time = mean(time), 
                           sd_time = sd(time))) %>% 
      add_col_in_nest(s_avg_time = if_else(avg_time > time_line, 1, 0)) %>% 
      add_col_in_nest(s_sd_time = if_else(sd_time > 0.175, 1, 0)) %>% 
      add_col_in_nest_byGRP(group_vars = vars({{location_exp}}), mutate_vars = vars(score = sum(s_avg_time, s_sd_time))) %>% 
      filter_in_nest(score >= 1)
    
    inferred_loc <- df_score %>% 
      extract_loc(vars(score), show_n_loc = show_n_loc, keep_score = F)
    inferred_loc
  } else if(recipe == "FREQ"){
    
    cleaned_df_byuser <- df_enrich %>%
      summ_in_nest(n_tweets = n(), 
                   n_locs = n_distinct(!!location_exp)) %>%
      remove_bots(user = user, counts = "n_tweets", topNpct_user = rm_pct) %>%
      filter_var(n_tweets > 10 & n_locs > 10)
    
    
    data_cols_nm <- cleaned_df_byuser$data[[1]] %>% names()
    nest_cols_nm <- data_cols_nm[-which(data_cols_nm %in% c(location_exp))]
    
    cleaned_df_byloc <- cleaned_df_byuser %>% 
      grpSumm_in_nest(nest_cols = nest_cols_nm, vars(n_tweets_loc = n())) %>% 
      filter_in_nest(n_tweets_loc > 10)
    
    inferred_loc <- cleaned_df_byloc %>% 
      add_col(empty_tb = map_lgl(data, plyr::empty)) %>% 
      filter_var(empty_tb == F) %>% 
      extract_loc(vars(n_tweets_loc), show_n_loc = show_n_loc, keep_score = F)
    inferred_loc
  }
}



