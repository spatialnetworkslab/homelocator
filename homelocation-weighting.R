## load library 
library(Hmisc) 
source("homelocation-filtering.R")

###########################################################################################
## expand variables 
home_filter <- sf_user_filter %>% unnest() %>% 
               left_join(., (sf_df %>% as_tibble() %>% select(c(u_id, GEOID, Datetime)))) %>%
               group_by(u_id) %>% 
               mutate(date = ymd_hms(Datetime),
                      counts = n(),
                      year = year(date),
                      month = month(date, label = TRUE),
                      day = day(date),
                      day_of_week = wday(date,label = TRUE),
                      hour_of_day = hour(date),
                      week = if_else(day_of_week %in% c("Sat", "Sun"), "weekend", "weekday"),
                      counts_group = cut2(counts, c(100, 1000, 2000, 3000, 4000, 5000, 10000, 15000, 25000)),
                      times_numeric = hour(date) + minute(date)/60 + second(date)/3600,
                      # daytimes = if_else(times_numeric >= 9 & times_numeric <= 18, "work time",
                      #                    ifelse(times_numeric > 18 & times_numeric <= 23, "night time", "bed time"))
                      daytimes = if_else(times_numeric >= 9 & times_numeric <= 18, "work time", "night time"))%>% 
               select(-c(Datetime, times_numeric)) %>% nest()
    

## Temporal Variables Effect 
### Counts 
home_extract_bycounts <- function(df){
    home_loc <- df %>% 
        group_by(GEOID) %>%
        summarise(counts = n()) %>% 
        top_n (n=1, wt = counts) %>% 
        slice(1) %>%
        pull(GEOID)
    home_loc
}

### Week (weekday and weekend) 
home_extract_byweek <- function(df) {
    home_loc <- df %>% 
        group_by(GEOID, week) %>% 
        summarise(count_week = n()) %>% 
        #mutate(percent_week = count_week/sum(count_week)) %>% 
        group_by(GEOID) %>% 
        summarise(mean = mean(count_week),
                  std = coalesce(sd(count_week),0)) %>% ## change sd na to 0
        top_n(n=1, wt = std) %>% 
        slice(1) %>% 
        pull(GEOID)
    if(length(home_loc) == 0) return(NA)
    home_loc
}

### daytimes (work time, night time) 
home_extract_bydaytimes <- function(df){
    home_loc <- df %>% 
        group_by(GEOID, daytimes) %>%
        summarise(count_daytimes = n()) %>% 
        mutate(percent_daytimes = count_daytimes/sum(count_daytimes)) %>% 
        group_by(GEOID) %>%
        mutate(mean = mean(count_daytimes),
               std = coalesce(sd(count_daytimes), 0)) 
    
    if (length(unique(home_loc$daytimes)) != 1) {
        home_loc_sub <- home_loc %>% select(c(GEOID, daytimes, percent_daytimes)) %>%
            spread(daytimes, percent_daytimes) %>% 
            filter(`night time` >= `work time`) %>% 
            select(c(GEOID))
        home_loc <- left_join(home_loc_sub, home_loc) %>% 
            ungroup() %>%
            top_n(n=1, wt=std) %>% 
            slice(1) %>% 
            pull(GEOID)
        
    } else {
        home_loc <- home_loc %>%
            ungroup() %>%
            top_n(n=1, wt=std) %>% 
            slice(1) %>% 
            pull(GEOID)
    }
    if(length(home_loc) == 0) return(NA)
    home_loc
}

### month
home_extract_bymonth <- function(df) {
    home_loc <- df %>% 
        group_by(GEOID, month) %>% 
        summarise(count_month = n()) %>% 
        #mutate(percent_month = count_month/sum(count_month)) %>% 
        group_by(GEOID) %>% 
        summarise(mean = mean(count_month),
                  std = coalesce(sd(count_month),0)) %>% 
        top_n(n=1, wt = std) %>% 
        slice(1) %>% 
        pull(GEOID)
    if(length(home_loc) == 0) return(NA)
    home_loc
}

### day
home_extract_byday <- function(df) {
    home_loc <- df %>% 
        group_by(GEOID, day) %>% 
        summarise(count_day = n()) %>% 
        #mutate(percent_day = count_day/sum(count_day)) %>% 
        group_by(GEOID) %>% 
        summarise(mean = mean(count_day),
                  std = coalesce(sd(count_day),0)) %>% 
        top_n(n=1, wt = std) %>% 
        slice(1) %>% 
        pull(GEOID)
    if(length(home_loc) == 0) return(NA)
    home_loc
}

home_loc <- home_filter %>% 
    mutate(homeloc_bycounts = future_map_chr(data, home_extract_bycounts),
           homeloc_byweek = future_map_chr(data, home_extract_byweek),
           homeloc_bydaytimes = future_map_chr(data, home_extract_bydaytimes),
           homeloc_bymonth = future_map_chr(data, home_extract_bymonth),
           homeloc_byday = future_map_chr(data, home_extract_byday)) %>% 
    select(-c(data)) 



##############################################################
## add group to home_loc in order to compare by group 
home_loc <- left_join(home_loc,home_filter %>% unnest() %>% select(c(u_id, counts_group)) %>% unique())
difference <- function(df, variable){
    different_output <- data_frame()
    home_imprecise <- df %>% select(c(u_id,homeloc_bycounts))
    home_precise <- df %>% select(c(u_id, variable))
    difference <- nrow(anti_join(home_imprecise, home_precise))
    if (difference == 0){
        print(paste("there is no different outputs for variable", variable))
    } else{
        home_imprecise <- home_imprecise %>% mutate(method = "bycounts")
        home_precise <- home_precise %>% mutate(method = variable)
        print(paste("The different output for variable", variable, "is as follows:"))
        print(cbind(home_precise, home_imprecise))
        different_output <- cbind(home_precise, home_imprecise)
    }
}
outputs_byweek <- difference(home_loc, "homeloc_byweek")
outputs_bydaytimes <- difference(home_loc, "homeloc_bydaytimes")
outputs_bymonth <- difference(home_loc, "homeloc_bymonth")
outputs_byday <-difference(home_loc, "homeloc_byday")













