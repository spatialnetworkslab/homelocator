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

- weighting: 
    - combine all info for each user, the info includes:
    ```u_id, GEOID, total_counts, count_tract, study_period, unique_days, months, days, hours,percent_weekend, percent_Sat_morning, percent_nighttime, counts_group```
     - give a weight to each variable and get a final score
     - change weight and monitor the change 

## load data
```{r}
library(tidycensus)
library(tidyverse)
library(data.table)
library(magrittr)
library(sf)
library(furrr)
library(dplyr)
library(tigris) 
library(lubridate)
library(Hmisc) 


df <- fread(system.file("extdata", "lexington.csv", package = "homelocator", mustWork = TRUE)) 
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% 
         select(-c(type,Time_offset))  

#users_initial <- unique(df$u_id)  ## initially there are 86710 users 


## add census tract to data 
census_api_key("your-api-key")
options(tigris_use_cache = TRUE, tigris_class="sf") 
acs_ky <- get_acs(state = "KY", geography = "tract", variables = "B19013_001", geometry = T)  %>% 
          st_transform(., "+init=epsg:4326") 
sf_df <- st_join(sf_df, acs_ky) %>% na.omit()
sf_user <- sf_df %>% as_tibble() %>% select(c(u_id, Datetime, GEOID)) %>% 
           group_by(u_id) %>% 
           nest() 
```







