



identify_home <- function(df, user = "u_id", timestamp = "created_at", location = "grid_id", recipe = "homelocator"){
  
  user <- rlang::sym(user) 
  timestamp <- rlang::sym(timestamp)
  location <- rlang::sym(location)
  recipe <- rlang::sym(recipe)
  
  df <- validate_dataset(df)
  df_nest <- nest_by_sglGp(df, group_var = !!user) %>% 
    derive_timestamp(., timestamp = !!timestamp)
  df_nest
  if(recipe == "homelocator"){
    df_filtered <- df_nest %>%
      summarise_var(n_tweets = n(),
                    n_locs = n_distinct(!!location)) %>%
      remove_bots(user = "u_id", counts = "n_tweets", top_u_percent = 0.01) %>%
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
  
}




