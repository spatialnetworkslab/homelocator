#' Extract home by total tweets counts.
#' 
#' Estimate a user's home location according to the total number of tweets. 
#' @param df A dataframe
home_extract_bycounts <- function(df){
    df %>% 
        select(c(GEOID, counts)) %>%
        top_n (n=1, wt = counts) %>% 
        slice(1) %>%
        pull(GEOID)
}


# home_loc_bycounts <- home_filter %>%
#     mutate(homeloc = future_map_chr(data, home_extract_bycounts)) %>%
#     select(-c(data))


#' Extract home by temporal variable score.
#' 
#' Estimate a user's home location according to the socre calculated by temporal varialbes. 
#' @param df A dataframe
home_extract <- function(df){
    df %>%
        select(c(GEOID, score)) %>%
        top_n (n=1, wt = score) %>%
        slice(1) %>%
        pull(GEOID)
}


# home_loc_byvar <- scored_results_combined %>%
#     mutate(homeloc = future_map_chr(data, home_extract)) %>%
#     select(-c(data))
