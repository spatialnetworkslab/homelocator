## add basic
mutate_time <- function(df, time_var){
  time_var_enq <- enquo(time_var)
  
  if (!is(df %>% pull(!!time_var_enq), "POSIXct")) {
    stop("Timestamp is not of class POSIXct")
  }
  
  df %>% 
    mutate(year = lubridate::year(!!time_var_enq),
          month = lubridate::month(!!time_var_enq),
          day = lubridate::day(!!time_var_enq), 
          day_of_week = lubridate::wday(!!time_var_enq),
          hour_of_day = lubridate::hour(!!time_var_enq), 
          date = as.Date(!!time_var_enq))
}

summarise_by_user <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  add_column <- . %>% 
    summarise(!!!adds_exp_enq)
  df %>% 
    mutate(adds = purrr::map(user_data, add_column)) %>% 
    unnest(adds)
}


#grouped by multiple variables 
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

# filter function 
filter_var <- function(df, filter_exp){
  filter_exp_enq <- enquo(filter_exp)
  df %>% 
    filter(!!filter_exp_enq)
}


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
# arrange the dataframe by (grouped) variable value 
arrange_var_by_group <- function(df, group_var, ...){
  group_var_enq <- enquo(group_var)
  arrange_exp_enq <- enquos(...)
  df %>% 
    group_by(!!group_var_enq) %>% 
    arrange(desc(!!!arrange_exp_enq)) 
}

arrange_var <- function(df, ...){
  arrange_exp_enq <- enquos(...)
  df %>% 
    arrange(desc(!!!arrange_exp_enq))
}

#remove certain percent top users to avoid bots 
remove_top_n <- function(df, top_user_percent){
  top_user_percent_enq <- enquo(top_user_percent)
  
  df %>% 
    dplyr::select(u_id, counts_per_user) %>% 
    unique() %>%
    dplyr::slice(round(n_distinct(.$u_id)*!!top_user_percent_enq):n()) %>%
    left_join(., df)
}

mutate_score_by_user <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  add_column <- . %>% 
    mutate(!!!adds_exp_enq)
  df %>% 
    mutate(adds = purrr::map(user_data, add_column)) %>% 
    unnest(adds) %>% 
    group_by(u_id) %>% 
    nest(.key = user_data)
}

extract_home <- function(df, score_var, ...){
  score_var_enq <- enquo(score_var)
  arrange_vars_enq <- enquos(...)
  
  get_loc <- . %>% 
    arrange(., desc(!!!arrange_vars_enq)) %>% 
    top_n(5) %>% 
    filter(!!score_var_enq) %>% 
    slice(1) %>% 
    pull(GEOID)
  
  df %>% 
    mutate(home = purrr::map(user_data, get_loc)) %>% 
    unnest(home)
}


#Apply to three methods 
#Data cleanning 
df <- read_csv("data/lexington-with-GEOID-2012-2017.csv") %>% 
  select(c(id, u_id, created_at, GEOID)) %>% 
  mutate_time(created_at) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) 

# qq
df_qq <- df %>% 
  summarise_by_user(counts_per_user = n(), distinct_loc_per_user = n_distinct(GEOID)) %>% 
  filter_var(counts_per_user > 10) %>% 
  filter_var(distinct_loc_per_user > 10) %>% 
  summarise_by_groups(vars(GEOID), 
                      vars(counts_per_loc = n(), 
                           distinct_hour_per_loc = n_distinct(hour_of_day), 
                           distinct_day_per_loc = n_distinct(date), 
                           time_period_per_loc = as.numeric(max(created_at) - min(created_at), "days"))) %>% 
  filter_var(counts_per_loc > 10) %>% 
  filter_var(distinct_hour_per_loc > 10) %>% 
  filter_var(distinct_day_per_loc > 10) %>% 
  filter_var(time_period_per_loc > 10) %>% 
  arrange_var(counts_per_user) %>% 
  remove_top_n(top_user_percent = 0.01) %>% 
  unnest()
  


df_qq_expanded <- df_qq %>% 
  add_column(week = if_else(day_of_week %in% c(1,7), 1, 2)) %>%  # 1 for weekend, 2 for weekday 
  add_column(numeric_time = lubridate::hour(created_at) + lubridate::minute(created_at)/60 + lubridate::second(created_at) / 3600) %>% 
  add_column(time_frame = if_else(numeric_time >= 9 & numeric_time <= 18, 2, 1)) %>% # 1 for rest time, 2 for work time 
  add_column(morning_time = if_else(numeric_time >= 6 & numeric_time <= 12, 1, 2)) # 1 for morning, 2 for afternoon and night 

home_qq <- df_qq_expanded %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  summarise_by_user(distinct_day_of_week = n_distinct(day_of_week), 
                    distinct_month = n_distinct(month)) %>% 
  summarise_by_groups(vars(week), 
                      vars(counts_week = n())) %>% 
  add_column_by_group(vars(u_id), 
                      vars(percent_week = counts_week/sum(counts_week))) %>% 
  summarise_by_groups(vars(time_frame), 
                      vars(counts_time_frame = n())) %>% 
  add_column_by_group(vars(u_id),
                      vars(percent_time_frame = counts_time_frame/sum(counts_time_frame))) %>% 
  mutate_score_by_user(score_counts_per_loc = 0.1 * (counts_per_loc/max(counts_per_loc))) %>% 
  mutate_score_by_user(score_distinct_hour_per_loc = 0.1 * (distinct_hour_per_loc/24)) %>% 
  mutate_score_by_user(score_distinct_day_per_loc = 0.1 * (distinct_day_per_loc/max(distinct_day_per_loc))) %>% 
  mutate_score_by_user(score_time_period_per_loc = 0.1 * (time_period_per_loc/max(as.numeric(time_period_per_loc)))) %>% 
  mutate_score_by_user(score_percent_week = 0.2 * (time_period_per_loc)) %>% 
  mutate_score_by_user(score_percent_time_frame = 0.2 * (percent_time_frame)) %>% 
  mutate_score_by_user(score_distinct_day_of_week = 0.1 * (distinct_day_of_week/7)) %>% 
  mutate_score_by_user(score_distinct_month = 0.1 * (distinct_month)) %>% 
  unnest() %>% 
  add_column(score = rowSums(.[, c("score_counts_per_loc", "score_distinct_hour_per_loc","score_distinct_day_per_loc","score_time_period_per_loc", "score_percent_week", 
                                             "score_percent_time_frame", "score_distinct_day_of_week", "score_distinct_month")])) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  extract_home(score > 0, score)
  
  
# ahas 
df_ahas <- df %>% 
  summarise_by_user(counts_per_user = n(), distinct_loc_per_user = n_distinct(GEOID)) %>% 
  summarise_by_groups(vars(GEOID), 
                      vars(counts_per_loc = n(), 
                           distinct_date_per_loc = n_distinct(date))) %>% 
  arrange_var_by_group(u_id, counts_per_loc, distinct_date_per_loc) %>% 
  top_n(., 1, counts_per_loc) %>%  # get top one loc
  unnest(grouped_data) %>% 
  group_by(u_id, counts_per_user) %>% 
  nest(.key = user_data) %>% 
  summarise_by_groups(vars(GEOID, year, month), 
                      vars(distinct_day_per_month = n_distinct(day))) %>% 
  filter_var(distinct_day_per_month > 7) %>% # top one loc must has at least 7 distinct day 
  select(u_id, counts_per_user) %>% 
  unique() %>% 
  left_join(., df) %>% 
  summarise_by_groups(vars(GEOID, year, month), 
                      vars(distinct_day_per_month = n_distinct(day))) %>% 
  filter_var(distinct_day_per_month > 2) %>% # each loc must has at least 2 distinct day 
  arrange_var(counts_per_user) %>% 
  remove_top_n(top_user_percent = 0.2) %>% # remove top 1% users  
  unnest()

time_line <- chron::times("17:00:00") %>% as.numeric()
home_ahas <- df_ahas %>% 
  add_column(time = format(created_at, format="%H:%M:%S") %>% chron::times()) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  summarise_by_groups(vars(GEOID),
                      vars(distinct_date_per_loc = n_distinct(date),
                           counts_per_loc = n(),
                           avg_time = mean(time),
                           sd_time = sd(time))) %>% 
  add_column(score_avg_time = if_else(avg_time > time_line, 1, 0)) %>% 
  add_column(score_sd_time = if_else(sd_time > 0.175, 1, 0)) %>% 
  add_column(score = rowSums(.[, c("score_avg_time", "score_sd_time")])) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  extract_home(score >= 1, distinct_date_per_loc, counts_per_loc)


# efs 
df_efs <- df %>% 
  summarise_by_user(counts_per_user = n(), 
                    distinct_loc_per_user = n_distinct(GEOID)) %>% 
  filter_var(distinct_loc_per_user > 3) %>% # remove lower user 
  arrange_var(counts_per_user) %>% 
  remove_top_n(top_user_percent = 0.01) %>% # remove top 1% user 
  unnest() %>% 
  filter_var(!day_of_week %in% c(1, 7)) # remove weekend data 


weight_rest <- mean(0.744, 0.735, 0.737)
weight_leisure <- mean(0.362, 0.357, 0.354)

home_efs <- df_efs %>% 
  add_column(time_frame = if_else(hour_of_day >= 2 & hour_of_day < 8, "Rest_time", if_else(hour_of_day >= 8 & hour_of_day < 19, "Active_time", "Leisure_time"))) %>% 
  filter_var(time_frame != "Active_time") %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  summarise_by_groups(vars(GEOID, date, time_frame), 
                      vars(counts_per_day = n())) %>% 
  spread(time_frame, counts_per_day) %>% 
  replace(., is.na(.), 0) %>% 
  add_column(weighted_counts = weight_rest * Rest_time + weight_leisure * Leisure_time) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  summarise_by_groups(vars(GEOID), 
                      vars(score = sum(weighted_counts))) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) %>% 
  extract_home(score > 0, score)
  