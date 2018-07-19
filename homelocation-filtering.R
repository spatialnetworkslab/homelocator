## load library
source("loading-data.R")

###############################################################################################
## home location filtering 
home_filtering <- function(df,counts = 20,count_tract = 10,study_period = 10,unique_days = 10,hours = 8) {
    home_filter <- df %>% 
                   mutate(date = ymd_hms(Datetime),
                          counts = n()) %>% 
                   filter(counts > 20) %>% 
                   group_by(GEOID) %>% 
                   summarise(count_tract = n(),
                             study_period = max(as.Date(Datetime))- min(as.Date(Datetime)),
                             unique_days = n_distinct(as.Date(date)),
                             hours = n_distinct(hour(date))) %>% 
                   filter(count_tract > 10 & study_period > 10 & unique_days > 10 & hours > 8)
}

sf_user <- sf_df %>% as_tibble() %>% select(c(u_id, Datetime, GEOID)) %>% 
           group_by(u_id) %>% 
           nest()

sf_user_filter <- sf_user %>%
    mutate( home_filter = future_map(data, home_filtering)) %>% select(-c(data)) 


# users <- sf_user_filter %>% unnest() %>% select(c(u_id)) %>% unique()  
## after filtering step, users decrease from 86710 to 11631 (13.4%)







