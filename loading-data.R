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
library(Hmisc) 
library(tictoc)

################################################################################################
## load data 
tic("loading data")
df <- fread("data/lexington-2012-2016.csv") 
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% select(-c(type,Time_offset))# convert data coordinate to geometry
users_initial <- unique(df$u_id)  ## initially there are 86710 users 

## add census tract to data 
census_api_key("2fcf1623c436882ad5e62a47280ab732d815e363")
options(tigris_use_cache = TRUE, tigris_class="sf") 
acs_ky <- get_acs(state = "KY", geography = "tract", variables = "B19013_001", geometry = T)  %>% # download Lexington Kentucky 2012-2016 Census tract
    st_transform(., "+init=epsg:4326") # transform acs_ky to the same crs as research ares (crs=4326)
sf_df <- st_join(sf_df, acs_ky) %>% na.omit()
sf_user <- sf_df %>% as_tibble() %>% select(c(u_id, Datetime, GEOID)) %>% 
    group_by(u_id) %>% 
    nest()
toc()