#load library
library(tidycensus)
library(tidyverse)
library(data.table)
library(magrittr)
library(sf)
library(furrr)
library(dplyr)
library(tigris) 
library(lubridate)

################################################################################################
## load data 
df <- fread("data/lexington-2012-2016.csv") 
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% select(-c(type,Time_offset))# convert data coordinate to geometry

## add census tract to data 
census_api_key("2fcf1623c436882ad5e62a47280ab732d815e363")
options(tigris_use_cache = TRUE, tigris_class="sf") 
acs_ky <- get_acs(state = "KY", geography = "tract", variables = "B19013_001", geometry = T)  %>% # download Lexington Kentucky 2012-2016 Census tract
          st_transform(., "+init=epsg:4326") # transform acs_ky to the same crs as research ares (crs=4326)
sf_df <- st_join(sf_df, acs_ky) %>% na.omit()

###############################################################################################
## home location filtering 
home_filtering <- function(df) {
    home_filter <- df %>% 
        mutate(date = ymd_hms(Datetime),
               counts = n()) %>% 
        filter(counts > 20) %>% 
        select(-c(counts)) %>% 
        group_by(GEOID) %>% 
        summarise(count_tract = n(),
                  study_period = max(as.Date(Datetime))- min(as.Date(Datetime)),
                  unique_days = n_distinct(as.Date(date)),
                  hours = n_distinct(hour(date))) %>% 
        filter(count_tract > 10 & study_period > 10 & unique_days > 10 & hours > 8) %>% 
        top_n(n=1, wt = count_tract) %>% 
        slice(1) %>% 
        pull(GEOID)
    if(length(home_filter) == 0) return(NA)
    home_filter
}

sf_user <- sf_df %>% as_tibble() %>% select(c(u_id, Datetime, GEOID)) %>% 
           group_by(u_id) %>% 
           nest()

sf_user_filter <- sf_user %>%
    mutate(home_filter = future_map_chr(data, home_filtering)) %>%
    select(-data)

