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
options(future.globals.maxSize = 768 * 1024^2)
df <- fread("data/lexington-2012-2018.csv") 
sf_df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant") %>% select(-c(type))# convert data coordinate to geometry
users_initial <- unique(df$u_id)  ## initially there are 95767 users 

## add census tract to data 
census_api_key("2fcf1623c436882ad5e62a47280ab732d815e363")
options(tigris_use_cache = TRUE, tigris_class="sf") 
acs_ky <- get_acs(state = "KY", geography = "tract", variables = "B19013_001", geometry = T)  %>% # download Lexington Kentucky 2012-2016 Census tract
    st_transform(., "+init=epsg:4326") # transform acs_ky to the same crs as research ares (crs=4326)
sf_df <- st_join(sf_df, acs_ky) %>% na.omit()
toc()

