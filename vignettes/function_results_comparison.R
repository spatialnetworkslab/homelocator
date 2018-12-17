get_neighbos <- function(ids){
  neighbor <- c()
  for (i in ids) {
    neighbor <- append(neighbor, neighbors[[i]]) %>% unique()
  }
  return(neighbor)
}
detect_neighbor <- function(data){
  df1_locIDs <- data %>% filter(homeloc == "homeloc_df1") %>% pull(id)
  df2_locIDs <-  data %>% filter(homeloc == "homeloc_df2") %>% pull(id)
  df1_neighbors <- get_neighbos(df1_locIDs)
  df2_neighbors <- get_neighbos(df2_locIDs)
  if (df2_locIDs %in% df1_neighbors){
    return("neighboring")
  } else if(df1_locIDs %in% df2_neighbors){
    return("neighboring")
  } else{
    return("non-neighboring")
  }
}
commonRes_detect <- function(df_1, df_2){
  # get common users ids 
  common_IDs <- intersect(df_1$u_id, df_2$u_id)
  print(paste("There are", common_IDs %>% length(), "common users from the two methods"))
  # get the same results from the common users 
  same_Res <- inner_join(df_1 %>% filter(u_id %in% common_IDs), df_2 %>% filter(u_id %in% common_IDs), by = c("u_id", "homeloc"))
  same_Res_IDs <- same_Res %>% pull(u_id) %>% n_distinct()
  print(paste("There are", same_Res_IDs, "users have the same results from the two methods"))
  return(same_Res)
}

diffRes_detect <- function(df_1, df_2, df, acs_ky){
  # get common users ids 
  common_IDs <- intersect(df_1$u_id, df_2$u_id)
  sameRes <- commonRes_detect(df_1, df_2)
  # get the different user results from the common users 
  diffRes <- setdiff(common_IDs, sameRes$u_id)
  print(paste("There are", diffRes %>% length(), "users have the different results from the two methods"))
  # look into details to check whether the two results of each user are neighboring cells or not 
  diff_df <- df_1 %>% 
    filter(u_id %in% diffRes) %>% 
    setNames(c("u_id", "homeloc_df1")) %>% 
    left_join(., (df_2 %>% filter(u_id %in% diffRes) %>% setNames(c("u_id", "homeloc_df2")))) %>% 
    gather("homeloc_df1", "homeloc_df2", key = "homeloc", value = "GEOID") %>% 
    left_join(., df) %>%
    mutate(times =  format(created_at, format="%H:%M:%S") %>% chron::times()) %>%
    group_by(u_id, homeloc, GEOID) %>%
    summarise(n_tweets = n(),
              sd = sd(times)) %>%
    ungroup() %>%
    arrange(., u_id)
  diff_df_2 <- diff_df %>%
    left_join(., acs_ky %>% st_set_geometry(NULL), by = c("GEOID")) %>%
    group_by(u_id) %>%
    nest() %>%
    mutate(neighbor_rel = future_map(data, detect_neighbor) %>% unlist()) %>%
    select(-data) %>%
    left_join(., diff_df, by = c("u_id")) %>%
    select(c(u_id, homeloc, GEOID, n_tweets, sd, neighbor_rel))
  print(paste("There are", diff_df_2 %>% filter(neighbor_rel == "neighboring") %>% pull(u_id) %>% n_distinct(), "users' different home results are neighboring cells from the two methods"))
  print(paste("There are", diff_df_2 %>% filter(neighbor_rel == "non-neighboring") %>% pull(u_id) %>% n_distinct(), "users' different home results are non-neighboring cells from the two methods"))
  diff_df_2
}

view_diffUsers <- function(df_diff, user_id){
  df <- df_diff %>% 
    left_join(., acs_ky) %>% 
    st_as_sf
  df %>% 
    filter(u_id %in% user_id) %>% 
    tm_shape() + 
    tm_fill("n_tweets") + 
    tm_text("homeloc", col = "black") + 
    tm_borders() + 
    tm_view(view.legend.position = c("left", "bottom")) +
    tm_facets(by = "u_id", ncol = 3)
}
