source("loading-data.R")

##############################################################################################
## home location filtering 
home_filtering <- function(df) {
    home_filter <- df %>% 
                   mutate(date = ymd_hms(Datetime),
                          counts = n()) %>% 
                   filter(counts > 20) %>% 
                   group_by(GEOID) %>% 
                   summarise(counts = n(),
                             study_period = max(as.Date(Datetime))- min(as.Date(Datetime)),
                             unique_days = n_distinct(as.Date(date)),
                             unique_hours = n_distinct(hour(date))) %>% 
                   filter(counts > 10 & study_period > 10 & unique_days > 10 & unique_hours > 8) 
}

tic("filtering")
sf_user_filter <- sf_user %>%
    mutate(home_filter = future_map(data, home_filtering)) %>% select(-c(data))
toc()
users <- sf_user_filter %>% unnest() %>% summarise(users = n_distinct(u_id))
## after filtering step, users decrease from 86710 to 11631 (13.4%)



## expand variables
home_filter <- sf_user_filter %>% unnest() %>%
    left_join(., (sf_df %>% as_tibble() %>% select(c(u_id, GEOID, Datetime)))) %>%  ## add Datetime var
    group_by(u_id) %>%
    mutate(date = ymd_hms(Datetime),
           total_counts = n(),
           year = year(Datetime),
           month = month(Datetime),
           day = day(Datetime),
           day_of_week = wday(date), # Sun is 1 and Sat is 7
           hour_of_day = hour(date),
           week = if_else(day_of_week %in% c(1, 7), 1, 2), ## 1 for weekend, 2 for weekday
           times_numeric = hour(date) + minute(date)/60 + second(date)/3600,
           daytimes = if_else(times_numeric >= 9 & times_numeric <= 18, 2, 1), # 1 for night time, 2 for work time
           times = if_else(times_numeric >= 6 & times_numeric <= 12, 1, 2), # 1 for morning, 2 for afternoon and night
           group = cut2(total_counts, c(100, 1000, 2000, 3000, 4000, 5000, 10000, 15000, 25000)))%>%
    select(-c(Datetime, times_numeric)) %>% nest()

##############################################################################################







