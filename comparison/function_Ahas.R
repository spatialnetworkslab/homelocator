read_data <- function(file){
  mainDir <- "/Users/qingqing/Dropbox/SUTD/Ate/homelocator/data/"
  df <- read_csv(paste0(mainDir, file))
  df_sub <- df %>% 
    select(id, u_id, created_at, GEOID) %>% 
    mutate(year = year(created_at), 
           month = month(created_at),
           day = day(created_at), 
           GEOID = as.character(GEOID)) 
  n_users <- df_sub$u_id %>% n_distinct()
  print(paste("Initially, there are", n_users, "users"))
  df_sub
}

get_RegularCells<- function(df){
  ## Regular cells-network cells: regularly visited by one person and from which the person has made calls on at least two different days a month, in our case, the cell treat as GEOID/location, the user should has sent tweets on the location at least two differnt days a month
  df_sub <- df %>% 
    group_by(u_id, GEOID, year, month) %>% 
    mutate(n_day_permonth = n_distinct(day)) %>%      ######### at least how many months ??????????????????????????????????
    ungroup() %>% 
    filter(n_day_permonth >= 2)
  n_users <- df_sub$u_id %>% n_distinct()
  print(paste("There are", n_users, "regular cells"))
  df_sub
}

## remove low users 
lowUsers_detect <- function(data){
  df <- data %>% 
    arrange(., desc(n_days), desc(n_tweets)) %>% 
    top_n(1) %>% 
    slice(1)
  
  if (df$n_days < 7){
    tibble::tibble(
      GEOID = NA, 
      n_days = NA, 
      n_tweets = NA)
  } else{
    data
  }
}

remove_toolowUsers <- function(df){
  df_sub <- df %>% 
    unite(date, year, month, day, sep = "-") %>% 
    mutate(date = as.Date(date)) %>% 
    group_by(u_id, GEOID) %>% 
    summarise(n_days = n_distinct(date), 
              n_tweets = n()) %>% 
    group_by(u_id) %>% 
    nest() %>% 
    mutate(results = future_map(data, lowUsers_detect)) %>% 
    select(-data) %>% 
    ungroup() %>% 
    unnest() %>% 
    na.omit() %>% 
    left_join(., df, by= c("u_id", "GEOID"))
  n_users <- df_sub$u_id %>% n_distinct()
  print(paste("After removing too low data, there are", n_users, "users remained"))
  df_sub
}


# remove too high users
remove_tooHighUsers <- function(df){
  # remove top 1% users 
  df_sub <- df %>% 
    group_by(u_id) %>% 
    summarise(total_counts = n()) %>% 
    ungroup() %>% 
    arrange(., desc(total_counts)) %>% 
    slice(round(n_distinct(.$u_id)*0.01):n()) %>% 
    left_join(., df) 
  n_user <- df_sub$u_id %>% n_distinct()
  print(paste("After removing too high data, there are", n_user, "users remained"))
  df_sub
}


get_topN_GEOID <- function(data, topN){
  data %>% 
    arrange(., desc(n_days), desc(n_tweets)) %>% 
    top_n(n=topN, wt = n_tweets) %>% 
    head(., n = topN) 
}
to.times <- function(x) chron::times(paste0(x, ":00"))

detect_anchorPoint <- function(data){
  data %>% 
    group_by(date) %>% 
    summarise(beginTime = mean(times)) %>% 
    ungroup() %>% 
    summarise(avg_beginTime = mean(beginTime), 
      sd_beginTime = sd(beginTime)) %>% 
    mutate(loc = if_else((avg_beginTime > to.times("17:00") & sd_beginTime > 0.175), "home", if_else((avg_beginTime < to.times("17:00") & sd_beginTime <= 0.175), "work", "not sure"))) %>%
    pull(loc)
}

determine_anchorType <- function(df){
  # get two regular cells/GEOID that had the highest number of days with calls 
  df_top2 <- regular_cells %>% 
    select(c(u_id, GEOID, n_days, n_tweets)) %>% 
    unique() %>% 
    group_by(u_id) %>% 
    arrange(., desc(n_days), desc(n_tweets)) %>% 
    nest() %>% 
    mutate(GEOID = future_map(data, function(x) get_topN_GEOID(x, 2))) %>% 
    select(-data) %>% 
    unnest() %>% 
    ungroup() %>% 
    select(-n_days, -n_tweets) %>% 
    left_join(., (df %>% select(c(u_id, GEOID, created_at))), by= c("u_id", "GEOID")) %>% 
    mutate(date = as.Date(created_at, format = "%Y-%m-%d"), 
      times = format(created_at, format="%H:%M:%S") %>% chron::times()) %>% 
    select(-created_at)
  
  # detect anchor type
  df_top2 %>% 
    group_by(u_id, GEOID) %>% 
    nest() %>% 
    mutate(anchor_type = future_map(data, detect_anchorPoint)) %>% 
    select(-data) %>% 
    unnest()
}




































