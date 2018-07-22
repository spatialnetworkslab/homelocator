## load library 
#source("homelocation-filtering.R")

###########################################################################################
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
## Temporal Variables Effect 
### Counts 
home_extract_bycounts <- function(df){
    home_loc <- df %>% 
        select(c(GEOID, counts)) %>%
        top_n (n=1, wt = counts) %>% 
        slice(1) %>%
        pull(GEOID)
    home_loc
}

home_loc_bycounts <- home_filter %>% 
    mutate(homeloc = future_map_chr(data, home_extract_bycounts)) %>% 
    select(-c(data)) 



##############################################################################################
### Week (weekday and weekend) 
score_week <- function(df){
    week_value <- df %>% 
        group_by(GEOID, week) %>% 
        summarise(counts = n()) %>%
        mutate(percent_week = counts/sum(counts)) %>% 
        group_by(GEOID) %>% 
        mutate(week_type = n()) %>% 
        filter(week_type == 2 | (week_type == 1 & week == 1)) %>% # remove tract only have weekday
        filter(week == 1) %>% ## only keep weekend percent value 
        select(GEOID, percent_week) %>% 
        setNames(c("GEOID","percent_weekend")) %>% list()
}


##############################################################################################
### Sat morning
score_sat_morning <- function(df){
    sat_morning_value <- df %>%
        filter(times == 1) %>% ## tweets in the morning
        group_by(GEOID, day_of_week) %>% 
        summarise(counts = n()) %>% 
        mutate(percent = counts/sum(counts)) %>% 
        filter(day_of_week == 7) %>% ## tweets on Sat
        select(GEOID, percent) %>% 
        setNames(c("GEOID", "percent_satMorning")) %>% 
        list()
}

##############################################################################################
### daytimes (work time, night time) 
score_daytimes <- function(df){
    daytimes_value <- df %>% 
        group_by(GEOID, daytimes) %>% 
        summarise(counts = n()) %>% 
        mutate(percent_daytime = counts/sum(counts)) %>% 
        filter(daytimes == 1 & percent_daytime >= 0.5) %>% 
        select(GEOID, percent_daytime) %>% 
        setNames(c("GEOID","percent_night")) %>%
        list()
}


##############################################################################################
### day
score_day <- function(df) {
    day_value <- df %>% 
        select(GEOID, day_of_week) %>% 
        group_by(GEOID) %>% 
        summarise(unique_dayofweek = n_distinct(day_of_week)) %>% 
        list()
}


##############################################################################################
### month
score_month <- function(df) {
    month_value <- df %>% 
        select(GEOID, month) %>% 
        group_by(GEOID) %>% 
        summarise(unique_months = n_distinct(month)) %>%
        list()
}


##############################################################################################
### combine temporal variables 
score <- home_filter %>% 
    mutate(var_week = future_map(data, score_week),
           var_sat_morning = future_map(data, score_sat_morning),
           var_daytimes = future_map(data, score_daytimes),
           var_day = future_map(data, score_day),
           var_month = future_map(data, score_month)) %>% 
    select(-c(data)) 


options(future.globals.maxSize = 768 * 1024^2)
combine_results <- function(score, num, home_filter){
        df <- merge(score[num, ]$var_week[[1]][[1]], score[num, ]$var_sat_morning[[1]][[1]], by = "GEOID", all=TRUE) %>% 
            merge(., score[num, ]$var_daytimes[[1]][[1]], by = "GEOID", all=TRUE) %>% 
            merge(., score[num, ]$var_day[[1]][[1]], by = "GEOID", all=TRUE) %>%
            merge(., score[num, ]$var_month[[1]][[1]], by = "GEOID", all=TRUE) 
        df <- df %>% 
              mutate(u_id = rep(score$u_id[num], nrow(df)))  ## add user id
        df_2 <- subset(home_filter, home_filter$u_id == score$u_id[num]) %>% select(c(data)) %>% unnest() %>%
                select(c(GEOID, total_counts, counts, study_period, unique_days, unique_hours, group)) %>% unique() ## add other info
        var_info <- suppressMessages(left_join(df,df_2)) %>%
                    select(c(u_id, GEOID, total_counts, counts, study_period, unique_days, unique_months, unique_dayofweek, unique_hours,
                             percent_weekend, percent_satMorning, percent_night, group)) ## order the variable
    }
   
terms <- c(1:nrow(score))
results <- future_map(terms, function(x) combine_results(score, x, home_filter)) 
