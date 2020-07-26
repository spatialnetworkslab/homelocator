
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
existing literature. A walkthrough demo video can be found in
`homelocator-package/demo.mov`.

## Installation

You can install the released version of homelocator with:

``` r
install.packages("~/Downloads/homelocator-package/homelocator_0.1.0.tar.gz", repos=NULL, type="source")
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
devtools::load_all(".")
#> Loading homelocator
#> Welcome to homelocator package!
library(homelocator)
test_sample <- read_csv(here("data/test_sample.csv"), col_types = cols(grid_id = col_character()))
df_validated <- validate_dataset(test_sample, 
                 user = "u_id", timestamp = "created_at", location = "grid_id")
#> 🎉 Congratulations!! Your dataset has passed validation.
#> 👤 There are 3000 unique users in your dataset.
#> 🌏 Now start your journey identifying their meaningful location(s)!
#> 👏 Good luck!
#> 
head(df_validated)
#> # A tibble: 6 x 3
#>       u_id grid_id created_at         
#>      <dbl> <chr>   <dttm>             
#> 1 10380249 400     2015-02-25 07:03:51
#> 2 52229026 1204    2014-05-20 05:17:21
#> 3 83978717 814     2013-10-24 03:13:22
#> 4 62462498 394     2014-01-09 08:57:39
#> 5   941995 535     2013-03-27 18:57:06
#> 6 67125054 757     2013-01-30 11:08:42
```

### Nesting users for parallel computing

To speed up computing progress, you can nest the validated dataset by
user so that the subsequent location inference can be applied to each
user at the same time.

``` r
df_nested <- nest_verbose(df_validated, c("created_at", "grid_id"))
#> 🛠 Start nesting...
#> ✅ Finish nesting!
#> ⌛ Nesting time: 0.343 secs
#> 
head(df_nested)
#> # A tibble: 6 x 2
#>       u_id data                
#>      <dbl> <list>              
#> 1 10380249 <tibble [216 × 2]>  
#> 2 52229026 <tibble [827 × 2]>  
#> 3 83978717 <tibble [562 × 2]>  
#> 4 62462498 <tibble [4,112 × 2]>
#> 5   941995 <tibble [322 × 2]>  
#> 6 67125054 <tibble [316 × 2]>
head(df_nested$data[[1]])
#> # A tibble: 6 x 2
#>   created_at          grid_id
#>   <dttm>              <chr>  
#> 1 2015-02-25 07:03:51 400    
#> 2 2015-09-29 14:42:34 561    
#> 3 2014-12-30 20:49:03 366    
#> 4 2016-05-27 15:44:55 574    
#> 5 2013-08-07 01:54:18 517    
#> 6 2015-05-02 07:10:31 854
```

### Enrich variables from timestamp

Add additional needed varialbes derived from the timestamp column. These
are often used/needed as intermediate variables in home location
algorithms, such as year, month, day, day of the week and hour of the
day, etc.

``` r
df_enriched <- enrich_timestamp(df_nested, timestamp = "created_at")
#> 🛠 Enriching variables from timestamp...
#> 
#> ✅ Finish enriching! New added variables: year, month, day, wday, hour, ymd.
#> ⌛ Enriching time: 2.462 secs
#> 
head(df_enriched$data[[1]])
#> # A tibble: 6 x 8
#>   created_at          grid_id  year month   day  wday  hour ymd       
#>   <dttm>              <chr>   <dbl> <dbl> <int> <dbl> <int> <date>    
#> 1 2015-02-25 07:03:51 400      2015     2    25     4     7 2015-02-25
#> 2 2015-09-29 14:42:34 561      2015     9    29     3    14 2015-09-29
#> 3 2014-12-30 20:49:03 366      2014    12    30     3    20 2014-12-31
#> 4 2016-05-27 15:44:55 574      2016     5    27     6    15 2016-05-27
#> 5 2013-08-07 01:54:18 517      2013     8     7     4     1 2013-08-07
#> 6 2015-05-02 07:10:31 854      2015     5     2     7     7 2015-05-02
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
