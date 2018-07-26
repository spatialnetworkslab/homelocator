# ## load library 
# #source("temporal-variables.R")
# 
# ##############################################################
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# find_difference <- function(df, group, variable){
#     different_output <- data_frame()
#     home_imprecise <- df %>% 
#         filter(counts_group == group) %>%
#         select(c(u_id,homeloc_bycounts)) %>% 
#         set_colnames(c("u_id", "home_loc"))
#     home_precise <- df %>% 
#         filter(counts_group == group) %>%
#         select(c(u_id, variable)) %>% 
#         set_colnames(c("u_id", "home_loc"))
#     
#     difference <- nrow(suppressMessages(anti_join(home_imprecise, home_precise)))
#     group_users <- nrow(home_precise)
#     if(difference != 0) {
#             print(paste("There are ", difference, "different homeloc for variable ", variable, "in group ", group, "with", group_users, "users"))
#         } else {
#             print(paste("There is no different homeloc for variable ", variable, "in group ", group, "with", group_users, "users"))
#         }
#         different_output <- suppressMessages(anti_join(home_imprecise, home_precise))
#         different_output
# }
# 
# groups <- c("[  100, 1000)","[ 1000, 2000)","[ 2000, 3000)", "[ 3000, 4000)", 
#             "[ 4000, 5000)", "[ 5000,10000)", "[10000,15000)", "[15000,25000]")
# 
# diff_byweek <- map(groups, function(x) find_difference(home_loc, x, "homeloc_byweek"))
# diff_bydaytimes <- map(groups, function(x) find_difference(home_loc, x, "homeloc_bydaytimes"))
# diff_bymonth <- map(groups, function(x) find_difference(home_loc, x, "homeloc_bymonth"))
# diff_byday <- map(groups, function(x) find_difference(home_loc, x, "homeloc_byday"))
# 
# 
# 
