#source("homelocation-filtering.R")

##############################################################################################
home_extract_bycounts <- function(df){
    home_loc <- df %>% 
        select(c(GEOID, counts)) %>%
        top_n (n=1, wt = counts) %>% 
        slice(1) %>%
        pull(GEOID)
    home_loc
}

##############################################################################################
### Week (weekday and weekend) 
calcu_week <- function(df){
    week_value <- df %>% 
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

##############################################################################################
### Sat morning
calcu_satMor <- function(df){
    sat_morning_value <- df %>%
        filter(times == 1) %>% ## tweets in the morning
        group_by(GEOID, day_of_week) %>% 
        summarise(counts = n()) %>% 
        mutate(percent = counts/sum(counts)) %>% 
        filter(day_of_week == 7) %>% ## tweets on Sat
        select(GEOID, percent) %>% 
        setNames(c("GEOID", "percent_satMor")) %>% 
        list()
}

##############################################################################################
### daytimes (work time, night time) 
calcu_daytimes <- function(df){
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
calcu_day <- function(df) {
    day_value <- df %>% 
        select(GEOID, day_of_week) %>% 
        group_by(GEOID) %>% 
        summarise(unique_dayofweek = n_distinct(day_of_week)) %>% 
        list()
}


##############################################################################################
### month
calcu_month <- function(df) {
    month_value <- df %>% 
        select(GEOID, month) %>% 
        group_by(GEOID) %>% 
        summarise(unique_months = n_distinct(month)) %>% 
        list()
}

##############################################################################################
### combine temporal variables 
tic("scoring")
para_values <- home_filter %>% 
    mutate(var_week = future_map(data, calcu_week),
           var_satMor = future_map(data, calcu_satMor),
           var_daytimes = future_map(data, calcu_daytimes),
           var_day = future_map(data, calcu_day),
           var_month = future_map(data, calcu_month)) %>% 
    select(-c(data)) 

options(future.globals.maxSize = 768 * 1024^2)
combine_values <- function(para_values, num, home_filter){
        df <- merge(para_values[num, ]$var_week[[1]][[1]], para_values[num, ]$var_satMor[[1]][[1]], by = "GEOID", all=TRUE) %>% 
            merge(., para_values[num, ]$var_daytimes[[1]][[1]], by = "GEOID", all=TRUE) %>% 
            merge(., para_values[num, ]$var_day[[1]][[1]], by = "GEOID", all=TRUE) %>%
            merge(., para_values[num, ]$var_month[[1]][[1]], by = "GEOID", all=TRUE) 
        df <- df %>% 
              mutate(u_id = rep(para_values$u_id[num], nrow(df)))  ## add user id
        df_2 <- subset(home_filter, home_filter$u_id == para_values$u_id[num]) %>% select(c(data)) %>% unnest() %>%
                select(c(GEOID, total_counts, counts, study_period, unique_days, unique_hours, group)) %>% unique() ## add other info
        var_info <- suppressMessages(left_join(df,df_2)) %>%
                    select(c(u_id, GEOID, total_counts, counts, study_period, unique_days, unique_months, unique_dayofweek, unique_hours,
                             percent_weekend, percent_satMor, percent_night, group)) %>% ## order the variable 
                    replace(., is.na(.), 0) ## replace na with 0
    }
   
terms <- c(1:nrow(para_values))
results <- future_map(terms, function(x) combine_values(para_values, x, home_filter)) 


scoring <- function(df) {
    scores <- df %>% 
        transmute(u_id = u_id,
                  GEOID = GEOID,
                  score_counts = counts/max(counts),
                  score_study_period = as.numeric(study_period)/max(as.numeric(study_period)),
                  score_unique_days = unique_days/max(unique_days),
                  score_months = unique_months/12,
                  score_unique_dayofweek = unique_dayofweek/7,
                  score_hours = unique_hours/max(unique_hours),
                  score_percent_weekend = percent_weekend/max(percent_weekend),
                  score_percent_satMor = percent_satMor/max(percent_satMor),
                  score_percent_night = percent_night/max(percent_night)) %>% 
        group_by(u_id, GEOID) %>% 
        summarise(final_score = sum(score_counts,score_study_period,score_unique_days,score_months,score_unique_dayofweek,score_hours,score_percent_weekend,score_percent_satMor,score_percent_night)) %>% 
        left_join(df, .) %>% 
        arrange(desc(final_score)) 
}

scored_results <- future_map(results, scoring)
toc()

# home_extract <- function(df){
#     home_loc <- df %>% 
#         select(c(GEOID, scores)) %>%
#         top_n (n=1, wt = scores) %>% 
#         slice(1) %>%
#         pull(GEOID)
#     home_loc
# }


















