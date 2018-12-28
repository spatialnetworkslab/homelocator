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
  





  

# test[1:3, ] %>% 
#   summarise_by_groups(vars(GEOID), 
#                       vars(counts_per_loc = n(),
#                            counts_hour = n_distinct(hour_of_day)))
# 
# test[1:3, ] %>% 
#   summarise_by_groups(vars(GEOID, year, month), 
#                       vars(counts_day_per_month = n_distinct(day)))

#nest by user, location 
# summarise_by_loc <- function(df, group_var, ...){
#   group_var_enq <- enquo(group_var)
#   adds_exp_enq <- enquos(..., .named = TRUE)
#   #adds_exp_nm <- names(adds_exp_enq)
#  
#   cal_column <- . %>% 
#     summarise(!!!adds_exp_enq)
#   
#   add_column <- . %>%
#     mutate(adds = purrr::map(loc_data, cal_column)) 
#   
#   df %>% 
#     mutate(user_data = purrr::map(user_data, ~.x %>% group_by(!!group_var_enq) %>% nest(.key = loc_data))) %>% 
#     mutate(vars = purrr::map(user_data, add_column)) %>% 
#     unnest(vars) %>% 
#     unnest(adds)
# }
## testing first 3 users 
# test[1:3, ] %>% 
#   summarise_by_user(counts_per_user = n(), counts_loc = n_distinct(GEOID)) %>% 
#   summarise_by_loc(GEOID, 
#     counts_per_loc = n(), 
#     counts_hour = n_distinct(hour_of_day), 
#     counts_day = n_distinct(date), 
#     counts_period = as.numeric(max(created_at) - min(created_at), "days"))
#   
# 
# summarise_user <- function(df, filter_exp){
#   filter_exp_enq <- enquo(filter_exp)
#   filter_exp_nm <- quo_name(filter_exp_enq)
#   
#   df %>% 
#     mutate(!!filter_exp_nm := map(data, function(x) summarise_var(x, counts_per_user)) %>% unlist())
# }
# df %>% 
#   mutate_time(timestamp = "created_at") %>%  ##add basic
#   nest(-u_id) %>%   ## nest by user
#   summarise_user(counts_per_user) %>% ## add computed variable
#   filter_var(counts_per_user > 10) ## do filtering
#   

