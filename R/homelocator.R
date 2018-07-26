#' homelocator: A package for estimating a twitter user's home location. 
#' 
#' 
#' The homlocator provides categories of important functions:
#' home_filtering, var_expand, combine_values, scoring, to_dataframe 
#' 
#' @section Home_filtering function:
#' The home_filtering function will discarde data that is too scarce to determine a home-like location.
#' 
#' @section Var_expand function:
#' The var_expand function will expand variables based on the timestamped variable 
#' 
#' @section Combine_values function:
#' The combine_values function will gather value of temporal varaibles: week, Sat morning, daytime, day, month
#' and combine with orginal info of the user for example: total tweets counts, counts in a tract, study period, 
#' unique days, unique hours and belonged group
#' 
#' @section Scoring function:
#' The Scoring function convert each value calculated before to 0-1 range, add up all variable score to get the 
#' final score of each tract, and order the tract based on the final score
#' 
#' @section To_dataframe function:
#' The to_dataframe function conver list result to a dataframe 
#' 
NULL