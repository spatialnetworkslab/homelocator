# Homelocator
Analysis a person's home location based on location and timestamped data

For each user:
- filtering:
    - total_tweets_counts > 20 
    - total_tweets_counts_per_tract > 20
    - study_period_per_tract > 10
    - unique_days_per_tract > 10
    - unique_hours_per_tract > 8
    
- temporal variables:
    - week (weekay & weekend): drop tract which only has tweets sent on weekday & calcualte the percentage of tweets sent on weekend
    - daytimes (work time & night time): drop tract which only has tweets sent on work time & keep the percentage of tweets sent on night time
    - month (Jan-Dec): calculate the distinct months that has tweets 
    - day (Mon-Sun): calculate the distinct day of week that has tweets 

- scoring: 
    - combine all info for each user, the info includes:
    ```u_id, GEOID, total_counts, counts, study_period, unique_days, unique_months, unique_dayofweek, unique_hours,percent_weekend, percent_satMor, percent_night, group```
     - give a score to each variable and add them together to get the final score for each user 
 

## load data
```{r}
df <- fread(system.file("extdata", "test_sample.csv", package = "homelocator", mustWork = TRUE)) 
home_filter <- var_expand(df)
users <- c(1:nrow(home_filter))
variable_values <- future_map(users, function(x) combine_values(home_filter, x)) 
home_score <- future_map(variable_values, scoring)
users_home <- to_dataframe(home_score) %>% 
    group_by(u_id) %>% 
    nest() %>% 
    mutate(homeloc = future_map_chr(data, home_extract)) %>% 
    select(-c(data))
```







