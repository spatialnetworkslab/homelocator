library(tidyverse)
library(sf)
library(here)

# generate grid neighbors
grids <- st_read(here("data-raw/MP14_SUBZONE_NO_SEA_PL.shp"), quiet = T) %>%
  st_transform(crs = 3414) %>%
  st_make_valid() %>%
  st_make_grid(., cellsize = 750, square = F) %>%
  st_sf() %>%
  rowid_to_column("grid_id")

st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
neighbors <- st_queen(grids)

# convert list to tibble
list_to_tibble <- function(index, neighbors) {
  tibble(grid_id = as.character(index)) %>%
    mutate(neighbor = list(neighbors[[index]]))
}
df_neighbors <- do.call(rbind, map(1:length(neighbors), function(x) list_to_tibble(x, neighbors)))

# saveRDS(df_neighbors, "data-raw/df_neighbors.rds")
save(df_neighbors, file = "data/df_neighbors.rda")


# generate sample file
df <- readRDS("~/GitProjects/identifyingmeaningfullocations/analysis/data/derived_data/sg_tweets_anonymized_20200929.rds")

df_nested <- df %>%
  dplyr::select(u_id_anonymized, created_at_sg_anonymized, grid_id) %>%
  dplyr::rename(u_id = u_id_anonymized, created_at = created_at_sg_anonymized) %>%
  nest(data = c(created_at, grid_id))

set.seed(1234)
test_sample <- df_nested %>%
  sample_n(size = 100) %>%
  unnest(cols = data) %>%
  sample_n(size = nrow(.))

save(test_sample, file = here("data/test_sample.rda"))


load("data/test_sample.rda")
