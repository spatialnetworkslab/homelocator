#' Tweets sent by 100 random users
#' 
#' De-identified test data includes 100 random users, 
#' and it can be used as an example to get started with homelocator package
#' 
#' @format A dataframe of 16,300 rows and 3 variables:
#' \describe{
#'   \item{u_id}{unique identifier for each user}
#'   \item{created_at}{specific timestamp for each data point in POSIXctformat}
#'   \item{grid_id}{unique identifier for each location}
#' }
"test_sample"


#' Neighbours for locations
#' 
#' Spatial neighbours for locations, where a neighbour has 
#' at least one line in common, but its interior does not intersect with the location
#' 
#' 
#' @format A nested dataframe of 1,942 rows and 2 variables:
#' \describe{
#'   \item{grid_id}{unique identifier for each location}
#'   \item{neighbor}{computed neighbours of the location with TOUCH relation}
#' }
"df_neighbors"
