#' Calculate week variable.
#' 
#' 
#' Calculate the percentage of weekday and weekend of each tract, throw away tract that only has tweets sent on 
#' weekday and keep weekend percent value and store the result in a list.
#' 
#' @param data A dataframe from a nested filtered dataframe
calcu_week <- function(data){
    data %>% 
        group_by(GEOID, week) %>% 
        summarise(counts = n()) %>%
        mutate(percent_week = counts/sum(counts)) %>% 
        group_by(GEOID) %>% 
        mutate(week_type = n()) %>% 
        filter(week_type == 2 | (week_type == 1 & week == 1)) %>% # remove tract only have weekday
        filter(week == 1) %>% ## only keep weekend percent value 
        select(GEOID, percent_week) %>% 
        setNames(c("GEOID","percent_weekend")) %>% 
        list()
}


#' Calculate Sat morning variable.
#' 
#' 
#' Calculate the percentage of tweets sent on Sat morning of each tract, filter tweets all sent in the morning first, 
#' calculate the percentage of tweets sent all days of a week and only keep the percentage that sent on Sat and store 
#' the result in a list. 
#' 
#' @inheritParams calcu_week
calcu_satmor <- function(data){
    data %>%
        filter(times == 1) %>% ## tweets in the morning
        group_by(GEOID, day_of_week) %>% 
        summarise(counts = n()) %>% 
        mutate(percent = counts/sum(counts)) %>% 
        filter(day_of_week == 7) %>% ## tweets on Sat
        select(GEOID, percent) %>% 
        setNames(c("GEOID", "percent_satMor")) %>% 
        list()
}


#' Calculate daytime variable.
#' 
#' 
#' Calculate the percentage of tweets sent on work time and night time of each tract, throw away tracts that percentage 
#' of work time is larger than that of night time, only keep percentage of night time and store the result in a list.
#' 
#' @inheritParams calcu_week
calcu_daytimes <- function(data){
    data %>% 
        group_by(GEOID, daytimes) %>% 
        summarise(counts = n()) %>% 
        mutate(percent_daytime = counts/sum(counts)) %>% 
        filter(daytimes == 1 & percent_daytime >= 0.5) %>% 
        select(GEOID, percent_daytime) %>% 
        setNames(c("GEOID","percent_night")) %>% 
        list()
}


#' Calculate day variable.
#' 
#' 
#' Calculate unique day of a week of each tract, and store the result in a list.
#' 
#' @inheritParams calcu_week
calcu_day <- function(data) {
    data %>% 
        select(GEOID, day_of_week) %>% 
        group_by(GEOID) %>% 
        summarise(unique_dayofweek = n_distinct(day_of_week)) %>% 
        list()
}


#' Calculate month variable.
#' 
#' 
#' Calculate unique month of a year of each tract, and store the result in a list.
#' 
#' @inheritParams calcu_week
calcu_month <- function(data) {
    data %>% 
        select(GEOID, month) %>% 
        group_by(GEOID) %>% 
        summarise(unique_months = n_distinct(month)) %>% 
        list()
}


#' Gather temporal variables' results.
#' 
#' 
#' Gather resutls get from funciton: calcu_week, calcu_satmor, calcu_daytimes, calcu_day, calcu_day, calcu_month 
#' and delete data dataframe 
#' 
#' @param df_filter A nested dataframe from filtering step
para_value <- function(df_filter){
    df_filter %>%
        mutate(var_week = future_map(data, calcu_week),
            var_satMor = future_map(data, calcu_satmor),
            var_daytimes = future_map(data, calcu_daytimes),
            var_day = future_map(data, calcu_day),
            var_month = future_map(data, calcu_month)) %>%
        select(-c(data))
}

#' Combine all info for each user.
#' 
#' 
#' Combine all info for each user including: u_id, GEOID, total_counts, counts, study_period, unique_days, unique_months, 
#' unique_dayofweek, unique_hours,percent_weekend, percent_satMor, percent_night, group and replace na with 0
#' 
#' @inheritParams para_value
#' @param num A number of users
combine_values <- function(df_filter, num){
    para_values <- para_value(df_filter)
    df <- merge(para_values[num, ]$var_week[[1]][[1]], para_values[num, ]$var_satMor[[1]][[1]], by = "GEOID", all=TRUE) %>%
        merge(., para_values[num, ]$var_daytimes[[1]][[1]], by = "GEOID", all=TRUE) %>%
        merge(., para_values[num, ]$var_day[[1]][[1]], by = "GEOID", all=TRUE) %>%
        merge(., para_values[num, ]$var_month[[1]][[1]], by = "GEOID", all=TRUE)
    df <- df %>%
        mutate(u_id = rep(para_values$u_id[num], nrow(df)))  ## add user id
    df_2 <- subset(df_filter, df_filter$u_id == para_values$u_id[num]) %>% select(c(data)) %>% unnest() %>%
        select(c(GEOID, total_counts, counts, study_period, unique_days, unique_hours, group)) %>% unique() ## add other info
    suppressMessages(left_join(df,df_2)) %>%
        select(c(u_id, GEOID, total_counts, counts, study_period, unique_days, unique_months, unique_dayofweek, unique_hours,
                 percent_weekend, percent_satMor, percent_night, group)) %>% ## order the variable
        replace(., is.na(.), 0) ## replace na with 0
}


#' Give a score to each tract for each user.
#' 
#' 
#' Give a 0-1 range score to each variable and add up them to get a final score for each tract, order the tracts 
#' according to the score. 
#' 
#' @param df_result A dataframe combined all user info
scoring <- function(df_result) {
    scores <- df_result %>% 
        transmute(u_id = u_id,
                  GEOID = GEOID,
                  score_counts = counts/max(counts),
                  score_study_period = as.numeric(study_period)/max(as.numeric(study_period)),
                  score_unique_days = unique_days/max(unique_days),
                  score_months = unique_months/12,
                  score_unique_dayofweek = unique_dayofweek/7,
                  score_hours = unique_hours/max(unique_hours),
                  score_percent_weekend = ifelse(max(percent_weekend) == 0, 0, percent_weekend/max(percent_weekend)),
                  score_percent_satMor = ifelse(max(percent_satMor) == 0, 0, percent_satMor/max(percent_satMor)),
                  score_percent_night = ifelse(max(percent_night) == 0, 0, percent_night/max(percent_night))) %>% 
        group_by(u_id, GEOID) %>% 
        summarise(score = sum(score_counts,score_study_period,score_unique_days,score_months,score_unique_dayofweek,
                              score_hours,score_percent_weekend,score_percent_satMor,score_percent_night)) 
    scores <- suppressMessages(left_join(df_result, scores)) %>% arrange(desc(score))
    scores
}


#' Convert list to dataframe.
#' 
#' Convert list resutls to a dataframe and nest it by user id. 
#
#' 
#' @param list_df A list results 
to_dataframe <- function(list_df) {
    df_total <- data_frame()
    for (i in list_df){
        df <- i
        df_total <- bind_rows(df_total, df)
    }
    return(df_total)
}





    











