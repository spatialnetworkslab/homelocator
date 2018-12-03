read_data <- function(file){
  mainDir <- "/Users/qingqing/Dropbox/SUTD/Ate/homelocator/data/"
  df <- read_csv(paste0(mainDir, file))
  df_sub <- df %>% 
    select(id, u_id, created_at, GEOID) %>% 
    mutate(year = year(created_at), 
      month = month(created_at),
      day = day(created_at),
      day_of_week = lubridate::wday(created_at, label = TRUE, abbr = TRUE),
      hour_of_day = hour(created_at), 
      time_frame = if_else(hour_of_day >= 2 & hour_of_day < 8, "Rest_time", if_else(hour_of_day >= 8 & hour_of_day < 19, "Active_time", "Leisure_time"))) 
  n_users <- df_sub$u_id %>% n_distinct()
  print(paste("Initially, there are", n_users))
  df_sub
}


remove_tooLowUsers <- function(df){
  # keep only users that have least one geo-tagged tweet from three areas of interest
  df_sub <- df %>%
    group_by(u_id) %>% 
    mutate(n_geoid = n_distinct(GEOID)) %>% 
    filter(n_geoid >= 3) %>% 
    ungroup() 
  n_user <- df_sub$u_id %>% n_distinct()
  print(paste("After removing too low data, there are", n_user, "remained"))
  df_sub
}


remove_tooHighUsers <- function(df){
  # remove top 1% users 
  df_sub <- df %>% 
    group_by(u_id) %>% 
    summarise(total_counts = n()) %>% 
    ungroup() %>% 
    arrange(., desc(total_counts)) %>% 
    slice(round(n_distinct(.$u_id)*0.01):n()) %>% 
    left_join(., df) %>% 
    select(-total_counts) 
  n_user <- df_sub$u_id %>% n_distinct()
  print(paste("After removing too high data, there are", n_user, "remained"))
  df_sub
}

visualize_data <- function(df){
  levels <- c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)
  cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  df %>% 
    group_by(day_of_week, hour_of_day) %>% 
    summarise(n_tweets = n()) %>% 
    group_by(day_of_week) %>% 
    mutate(total_tweets = sum(n_tweets), 
      freq = n_tweets/total_tweets) %>% 
    ungroup() %>% 
    ggplot(., aes(x=hour_of_day, y = freq, color = day_of_week)) +
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = 0:23, labels = levels) +
    scale_colour_manual(values=cbPalette) + 
    geom_vline(xintercept = 2, linetype="dotted") + 
    geom_text(aes(x = 0.5, y=0.075,  label="Leisure"), colour="black") + 
    geom_vline(xintercept = 8, linetype="dotted") + 
    geom_text(aes(x = 5, y=0.075,  label="Rest"), colour="black") + 
    geom_vline(xintercept = 19, linetype="dotted") + 
    geom_text(aes(x = 13, y=0.075,  label="Active"), colour="black") + 
    geom_text(aes(x = 21.5, y=0.075,  label="Leisure"), colour="black") + 
    theme(legend.position = c(0.9, 0.3),
      panel.background = element_blank(),
      axis.line = element_line("black")) + 
    labs(
      x = "Hours of the day", 
      y = "Tweet Frequency", 
      color = "Days"
    )
}

remove_weekendAct <- function(df){
  # Because there is a slight shift in the tweeting activity of the users during weekends, so we decide to ignore weekend acctivity when searching for the users' home and work location.
  df_sub <- df %>% 
    filter(!day_of_week %in% c("Sun", "Sat"))  
  n_user <- df_sub$u_id %>% n_distinct()
  print(paste("After removing weekend data, there are", n_user, "remained"))
  df_sub
}



extract_workloc <- function(data){
  data %>%
    mutate(GEOID = as.character(GEOID)) %>% 
    group_by(GEOID) %>% 
    summarise(counts = n()) %>% 
    ungroup() %>% 
    top_n(n=1, wt = counts) %>% 
    slice(1) %>% 
    pull(GEOID) 
}

# estimate the HOME and WORK locations of the user by finding the most "POPULAR" location during "non-working" (rest tiem & leisure time) and "working" (active) hours, respectively
identify_workloc <- function(df){
  # split the cleaned dataset into three subset according to the time frame 
  df_active <- df %>% filter(time_frame == "Active_time")
  ## Work location: calculates the most popular place, in number of unique days, among all the places the user tweeted during the Active timeframe 
  work_loc <- df_active %>% 
    group_by(u_id) %>% 
    nest() %>% 
    mutate(work_loc = future_map(data, extract_workloc) %>% unlist()) %>% 
    select(-data) %>% 
    ungroup()
  n_user <- work_loc$u_id %>% n_distinct()
  print(paste("Based on the model, we can identify", n_user, "users' work location"))
  work_loc
}


extract_homeloc <- function(data){
  weight_rest <- mean(0.744, 0.735, 0.737)
  weight_leisure <- mean(0.362, 0.357, 0.354)
  data %>% 
    mutate(weighted_counts = weight_rest * Rest_time + weight_leisure * Leisure_time,
      GEOID = as.character(GEOID)) %>% 
    group_by(GEOID) %>% 
    summarise(sums = sum(weighted_counts)) %>% 
    ungroup() %>% 
    top_n(n=1, wt=sums) %>% 
    slice(1) %>% 
    pull(GEOID)
}

identify_homeloc <- function(df){
  # Home location: calculates the most popular place, in number of unique days, among all the places the user tweeted during both the rest time and leisure timeframe; And users tweet from Home with higher probability during rest time, so apply a different weight Wr to the popularity of a place if the tweet is included in rest time, and Wl if the tweet is included in Leisute time; calculate the weights by estimating the average, amongst all users, fraction of tweets from the home location over the total number of tweets during the two different timeframes.
  df_rest_leisure <- df %>% 
    filter(time_frame != "Active_time") %>% 
    unite(unique_day, year, month, day, sep = "-") %>% 
    group_by(u_id, GEOID, unique_day, time_frame) %>% 
    summarise(counts = n()) %>% 
    ungroup() %>% 
    spread(time_frame, counts) %>% 
    replace(., is.na(.), 0)
  
  home_loc <- df_rest_leisure %>% 
    group_by(u_id) %>% 
    nest() %>% 
    mutate(home_loc = future_map(data, extract_homeloc) %>% unlist()) %>% 
    select(-data) %>% 
    ungroup()
  n_user <- home_loc$u_id %>% n_distinct()
  print(paste("Based on the model, we can identify", n_user, "users' home location"))
  home_loc
}














