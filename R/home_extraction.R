#' Extract home by total tweets counts.
#' 
#' Estimate a user's home location according to the total number of tweets. 
#' @inheritParams calcu_week
home_extract_bycounts <- function(data){
    data %>% 
        select(c(GEOID, counts)) %>%
        mutate(GEOID = as.character(GEOID)) %>%
        top_n (n=1, wt = counts) %>% 
        slice(1) %>%
        pull(GEOID)
}


#' Extract home by temporal variable score.
#' 
#' Estimate a user's home location according to the socre calculated by temporal varialbes. 
#' @param result_data A dataframe
home_extract <- function(result_data){
    result_data %>%
        select(c("GEOID", "score")) %>%
        mutate(GEOID = as.character(GEOID)) %>%
        top_n (n=1, wt = score) %>%
        slice(1) %>%
        pull(GEOID)
}

