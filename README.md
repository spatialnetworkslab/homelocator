
# homelocator

<!-- badges: start -->

<!-- badges: end -->

## Overview

The goal of `homelocator` is to provide a consistent framework and
interface for the adoption of different approaches for identifying
meaningful locations for users. With the package, you are able to write
structured, algorithmic ‘recipes’ to identify meaningful locations
according to your research requirements. The package also has a number
of built-in ‘recipes’ that have been translated from approaches in the
existing literature.

## Installation

You can install the released version of homelocator from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("homelocator")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("spatialnetworkslab/homelocator")
```

## Example

These are some basic examples that show you how to use common functions
in the package.

### Validate input dataset

You need to make sure the input dataset includes three essential
attributes:

  - a unique identifier for the person or user
  - a unique identifier for the spatial locaiton for the data point
  - a timestamp that relects the time the data point was created

You can use `validate_dataset()` to validate your input dataset before
starting identifying meaningful locations. In this function, you need to
specify the names of three essential attribute that used in your
dataset.

``` r
library(homelocator)
test_sample <- readRDS(here("data/test_sample.rds"))
df_validated <- validate_dataset(test_sample, 
                 user = "u_id", timestamp = "created_at", location = "grid_id")
#> 🎉 Congratulations!! Your dataset has passed validation.
#> 👤 There are 3000 unique users in your dataset.
#> 🌏 Now start your journey identifying their meaningful location(s)!
#> 👏 Good luck!
#> 
head(df_validated)
#> # A tibble: 6 x 3
#>   u_id     grid_id created_at         
#>   <chr>      <int> <dttm>             
#> 1 26295661     566 2013-03-05 22:03:15
#> 2 69400017    1012 2013-06-11 14:12:45
#> 3 16294855    1188 2013-04-30 18:47:58
#> 4 40275983     254 2014-02-15 16:14:01
#> 5 91759535    1233 2014-03-05 14:57:07
#> 6 81298067    1296 2015-04-22 05:30:41
```

### Nesting users for parallel computing

To speed up computing progress, you can nest the validated dataset by
user so that the subsequent location inference can be applied to each
user at the same time.

``` r
df_nested <- nest_verbose(df_validated, c("created_at", "grid_id"))
#> 🛠 Start nesting...
#> ✅ Finish nesting!
#> ⌛ Nesting time: 0.01 mins
#> 
head(df_nested)
#> # A tibble: 6 x 2
#>   u_id     data                
#>   <chr>    <list>              
#> 1 26295661 <tibble [406 × 2]>  
#> 2 69400017 <tibble [79 × 2]>   
#> 3 16294855 <tibble [5,889 × 2]>
#> 4 40275983 <tibble [67 × 2]>   
#> 5 91759535 <tibble [1,823 × 2]>
#> 6 81298067 <tibble [191 × 2]>
```

### Enrich variables from timestamp

Add additional needed varialbes derived from the timestamp column. These
are often used/needed as intermediate variables in home location
algorithms, such as year, month, day, day of the week and hour of the
day, etc.

``` r
enrich_timestamp(df_nested, timestamp = "created_at")
#> 🛠 Enriching variables from timestamp...
#> 
#> ✅ Finish enriching! New added variables: year, month, day, wday, hour, ymd.
#> ⌛ Enriching time: 0.05 mins
#> 
#> # A tibble: 3,000 x 2
#>    u_id     data                
#>    <chr>    <list>              
#>  1 26295661 <tibble [406 × 8]>  
#>  2 69400017 <tibble [79 × 8]>   
#>  3 16294855 <tibble [5,889 × 8]>
#>  4 40275983 <tibble [67 × 8]>   
#>  5 91759535 <tibble [1,823 × 8]>
#>  6 81298067 <tibble [191 × 8]>  
#>  7 45267153 <tibble [57 × 8]>   
#>  8 37588593 <tibble [4,475 × 8]>
#>  9 81235774 <tibble [335 × 8]>  
#> 10 67861300 <tibble [29 × 8]>   
#> # … with 2,990 more rows
```

### Use built-in recipes

Current available recipes, where `HMLC` is the default recipe used in
`identify_location`:

  - `HMLC`:
      - Weighs data points across multiple time frames to ‘score’
        potentially meaningful locations for each user
  - `FREQ`
      - Selects the most frequently ‘visited’ location assuming a user
        is active mainly around their home location.
  - `OSNA`: [Efstathiades et
    al.2015](https://www.researchgate.net/publication/279884727_Identification_of_Key_Locations_based_on_Online_Social_Network_Activity)
      - Finds the most ‘popular’ location during ‘rest’, ‘active’ and
        ‘leisure time. Here we focus on ’rest’ and ‘leisure’ time to
        find the most possible home location for each user.
  - `APDM`: [Ahas et
    al. 2010](https://www.researchgate.net/publication/233197970_Using_Mobile_Positioning_Data_to_Model_Locations_Meaningful_to_Users_of_Mobile_Phones)
      - Calculates the average and standard deviation of start time data
        points by a single user, in a single location.

<!-- end list -->

``` r
# default recipe: homelocator -- HMLC
identify_location(test_sample, user = "u_id", timestamp = "created_at", 
                  location = "grid_id", show_n_loc = 1, recipe = "HMLC")


# recipe: Frequency -- FREQ
identify_location(test_sample, user = "u_id", timestamp = "created_at", 
                  location = "grid_id", show_n_loc = 1, recipe = "FREQ")


# recipe: Online Social Network Activity -- OSNA
identify_location(test_sample, user = "u_id", timestamp = "created_at", 
                  location = "grid_id", show_n_loc = 1, recipe = "OSNA")


# recipe: Online Social Network Activity -- APDM
## APDM recipe strictly returns the most likely home location
## It is important to load the neighbors table before you use the recipe!!
## example: st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
##          neighbors <- st_queen(df_sf) ===> convert result to dataframe 

df_neighbors <- readRDS(here("data/neighbors.rds"))
identify_location(test_sample, user = "u_id", timestamp = "created_at", 
                  location = "grid_id", recipe = "APDM")
```
