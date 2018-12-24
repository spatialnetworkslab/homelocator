df <- read_csv("data/lexington-with-GEOID-2012-2017.csv") %>% 
  select(c(id, u_id, created_at, GEOID)) 

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

# nest by user 
test <- df %>% 
  mutate_time(created_at) %>% 
  group_by(u_id) %>% 
  nest(.key = user_data) 


summarise_by_user <- function(df, ...){
  adds_exp_enq <- enquos(..., .named = TRUE)
  #adds_exp_nm <- names(adds_exp_enq)
  
  add_column <- . %>% 
    summarise(!!!adds_exp_enq)
  
  df %>% 
    mutate(adds = purrr::map(user_data, add_column)) %>% 
    unnest(adds)
}

# testing first 3 users 
test[1:3, ] %>% 
  summarise_by_user(counts_per_user = n(), counts_loc = n_distinct(GEOID)) 


#nest by user, location 
summarise_by_loc <- function(df, group_var, ...){
  group_var_enq <- enquo(group_var)
  adds_exp_enq <- enquos(..., .named = TRUE)
  #adds_exp_nm <- names(adds_exp_enq)
 
  cal_column <- . %>% 
    summarise(!!!adds_exp_enq)
  
  add_column <- . %>%
    mutate(adds = purrr::map(loc_data, cal_column)) 
  
  df %>% 
    mutate(user_data = purrr::map(user_data, ~.x %>% group_by(!!group_var_enq) %>% nest(.key = loc_data))) %>% 
    mutate(vars = purrr::map(user_data, add_column)) %>% 
    unnest(vars) %>% 
    unnest(adds)
}

## testing first 3 users 
test[1:3, ] %>% 
  summarise_by_user(counts_per_user = n(), counts_loc = n_distinct(GEOID)) %>% 
  summarise_by_loc(GEOID, 
    counts_per_loc = n(), 
    counts_hour = n_distinct(hour_of_day), 
    counts_day = n_distinct(date), 
    counts_period = as.numeric(max(created_at) - min(created_at), "days"))


# filter function 
filter_var <- function(df, filter_exp){
  filter_exp_enq <- enquo(filter_exp)
  df %>% 
    filter(!!filter_exp_enq)
}





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

