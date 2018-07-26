.onAttach <- function(libname, pkgname){
    packageStartupMessage("Welcome to my package")
}

.onLoad <- function(libname, pkgname){
    op <- options()
    op.devtools <- list(
        devtools.path = "~/R-dev",
        devtools.install.args = "",
        devtools.name = "your name goes here",
        devtools.desc.author = 'person("First","Second", "first.last@example.com", role = c("aut","cre"))',
        devtools.desc.license = "What license is it under?",
        devtools.desc.suggests = NULL,
        devtools.desc = list()
    )
    toset <- !(names(op.devtools) %in% names(op))
    if(any(toset)) options(op.devtools[toset])
    invisible()
}

#' Filter scared data.
#' 
#' Kepp users have more than 20 tweets totally, and have more than 10 tweets, more than 10 unique days and more than 8 hours tweets in a tract record.
#' 
#' 
#' @param df A dataframe with timestamped variable
#' @example 
home_filtering <- function(df) {
    df %>% 
        mutate(date = ymd_hms(created_at),
               total_counts = n()) %>% 
        filter(total_counts > 20) %>% 
        group_by(GEOID) %>% 
        mutate(counts = n(),
               study_period = max(as.Date(created_at)) - min(as.Date(created_at)),
               unique_days = n_distinct(as.Date(date)),
               unique_hours = n_distinct(hour(date))) %>% 
        filter(counts > 10 & study_period > 10 & unique_days > 10 & unique_hours > 8) %>%
        select(-c(created_at))
}


#' Expand variables to dataset.
#' 
#' 
#' Add "year, month, day, day of week, hour of day, week, daytimes, times and group columnes to dataset. 
#' "year, month, day, day of week, hour of day, week, daytimes, times" are produced from timestamped variable,
#' and group is produced from total tweets counts according to diffrent range.
#' 
#' @param sf_df A nested dataframe
#' @return A nested expanded dataframe 
#' @example 
var_expand <- function(sf_df){
    sf_df %>% as_tibble() %>% select(c(u_id, created_at, GEOID)) %>% 
        group_by(u_id) %>% 
        nest() %>% 
        mutate(data = future_map(data, home_filtering)) %>%
        unnest() %>%
        group_by(u_id) %>%
        mutate(total_counts = n(),
               year = year(date),
               month = month(date),
               day = day(date),
               day_of_week = wday(date), # Sun is 1 and Sat is 7
               hour_of_day = hour(date),
               week = if_else(day_of_week %in% c(1, 7), 1, 2), ## 1 for weekend, 2 for weekday
               times_numeric = hour(date) + minute(date) / 60 + second(date) / 3600,
               daytimes = if_else(times_numeric >= 9 & times_numeric <= 18, 2, 1), # 1 for night time, 2 for work time
               times = if_else(times_numeric >= 6 & times_numeric <= 12, 1, 2), # 1 for morning, 2 for afternoon and night
               group = cut2(total_counts, c(0, 100, 1000, 2000, 3000, 4000, 5000, 10000, 15000, 25000))) %>%
        select(-c(date, times_numeric)) %>% nest()
}








