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
    mutate(n_day_permonth = n_distinct(day)) %>%      
    ungroup() %>% 
    filter(n_day_permonth >= 2) 
  n_users <- df_sub$u_id %>% n_distinct()
  print(paste("After removing random cells, there are", n_users, "users remained."))
  df_sub
}

## remove low users 
detect_lowUsers <- function(data){
  df <- data %>% 
    select(GEOID, year, month, n_day_permonth) %>% 
    unite(time, year, month, sep = "-") %>% 
    unique()
  days <- data %>% 
    group_by(GEOID) %>% 
    unite(date, year, month, day, sep = "-") %>% 
    mutate(date = as.Date(date)) %>% 
    group_by(GEOID) %>% 
    summarise(n_days = n_distinct(date), 
      n_tweets = n_distinct(id)) %>% 
    arrange(., desc(n_days), desc(n_tweets)) %>% 
    top_n(1) %>% 
    slice(1) %>% 
    left_join(., df) %>% 
    arrange(., desc(n_day_permonth)) %>% 
    top_n(1) %>% 
    slice(1) %>% 
    pull(n_day_permonth)
  if(days >= 7){
    return(1)
  } else{
    return(0)
  }
}

remove_lowUsers <- function(df){
  initial_users <- df$u_id %>% n_distinct()
  removed_users <- df %>% 
    group_by(u_id) %>% 
    nest() %>% 
    mutate(result = future_map(data, detect_lowUsers)) %>% 
    select(-data) %>% 
    ungroup %>% 
    unnest() %>% 
    filter(result == 0) %>% 
    pull(u_id)
  left_users <- initial_users - removed_users %>% n_distinct()
  print(paste("After remonving the too low users, there are", left_users, "users remained."))
  removed_users
}

# remove too high users
remove_highUsers <- function(df){
  # remove top 1% users 
  initial_users <- df$u_id %>% n_distinct()
  removed_users <- df %>% 
    group_by(u_id) %>% 
    summarise(total_counts = n()) %>% 
    ungroup() %>% 
    arrange(., desc(total_counts)) %>% 
    top_n(round(n_distinct(.$u_id)*0.01)) %>% 
    pull(u_id)
  n_user <- initial_users - removed_users %>% n_distinct()
  print(paste("After removing too high users, there are", n_user, "users remained."))
  removed_users
}

# detect single anchor location type 
detect_singleAnchor <- function(df){
  df %>% 
    group_by(u_id) %>% 
    mutate(n_geo = n_distinct(GEOID)) %>% 
    filter(n_geo == 1) %>% 
    select(-n_geo) %>% 
    ungroup() %>% 
    mutate(time = format(created_at, format="%H:%M:%S") %>% chron::times()) %>% 
    group_by(u_id, GEOID) %>% 
    summarise(avg_time = mean(time), 
      sd_time = sd(time)) %>% 
    mutate(loc = if_else(avg_time > to.times("17:00"), "home", if_else(avg_time <= to.times("17:00") & sd_time > 0.175, "home", "work"))) %>% 
    ungroup()
}

to.times <- function(x) chron::times(paste0(x, ":00"))
detect_multiAnchor <- function(data){
  data <- data %>% 
    mutate(time = format(created_at, format="%H:%M:%S") %>% chron::times()) %>% 
    unite(date, year, month, day, sep = "-") %>% 
    group_by(GEOID) %>% 
    summarise(n_days = n_distinct(date), 
      n_tweets = n(), 
      avg_time = mean(time), 
      sd_time = sd(time)) %>% 
    mutate(loc = if_else(avg_time > to.times("17:00"), "home", if_else(avg_time <= to.times("17:00") & sd_time > 0.175, "home", "work"))) %>% 
    ungroup() %>% 
    arrange(., desc(n_days), desc(n_tweets)) %>% 
    top_n(n = 5) %>% 
    head(5) %>% left_join(., acs_ky)
  
  GEOIDs <- data$GEOID
  locs <- data$loc
  IDs <- data$id
  len <- nrow(data)
  i <- 1
  while (i < len) {
    if(locs[1] == locs[i+1]){
      if(IDs[i+1] %in% neighbors[[IDs[1]]]){
        i <- i + 1
        if(i == len){
          result <- tibble(
            GEOID = c(GEOIDs[1], GEOIDs[2]),
            loc = c(locs[1], locs[2]),
            type = c(rep("one type, neighbor", 2)))
          break
        } else{next}
      } else{
        result <- tibble(
          GEOID = c(GEOIDs[1], GEOIDs[i+1]),
          loc = c(locs[1], locs[i+1]),
          type = c(rep("one type, non neighbor", 2)))
        break
      }
    } else{
      result <- tibble(
        GEOID = c(GEOIDs[1], GEOIDs[i+1]),
        loc = c(locs[1], locs[i+1]),
        type = c(rep("one home, one work", 2)))
      break
    }
  }
  return(result)
}

decide_multiAnchor <- function(data){
  df_type <- data %>% select(GEOID, loc, type) %>% unique()
  df_percent <- data %>% 
    group_by(GEOID) %>% 
    summarise(n_day = n_distinct(date)) %>% 
    ungroup() %>% 
    mutate(total_day = sum(n_day), 
      percent_day = n_day/total_day) %>% 
    select(GEOID, percent_day) 
  percentages <- df_percent$percent_day
  if(all(percentages <= 0.75)){
    df_type
  } else{
    df_type %>% head(1) %>% mutate(type = "multifunctional")
  }
}


