
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

You can install the released version of homelocator with:

``` r
install.packages("~/Desktop/homelocator/homelocator_0.1.0.tar.gz", repos=NULL, type="source")
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
#> 1 39592393    1376 2014-03-01 11:57:54
#> 2 68320978     944 2014-11-19 06:58:04
#> 3 85194110     789 2012-10-01 10:38:43
#> 4 66559308    1615 2014-08-07 02:58:43
#> 5 40221447     849 2012-11-13 11:32:36
#> 6 54795328     999 2013-08-01 10:06:44
```

### Nesting users for parallel computing

To speed up computing progress, you can nest the validated dataset by
user so that the subsequent location inference can be applied to each
user at the same time.

``` r
df_nested <- nest_verbose(df_validated, c("created_at", "grid_id"))
#> 🛠 Start nesting...
#> ✅ Finish nesting!
#> ⌛ Nesting time: 0.351 secs
#> 
head(df_nested)
#> # A tibble: 6 x 2
#>   u_id     data                
#>   <chr>    <list>              
#> 1 39592393 <tibble [100 × 2]>  
#> 2 68320978 <tibble [835 × 2]>  
#> 3 85194110 <tibble [805 × 2]>  
#> 4 66559308 <tibble [3,305 × 2]>
#> 5 40221447 <tibble [6,641 × 2]>
#> 6 54795328 <tibble [79 × 2]>
head(df_nested$data[[1]])
#> # A tibble: 6 x 2
#>   created_at          grid_id
#>   <dttm>                <int>
#> 1 2014-03-01 11:57:54    1376
#> 2 2012-11-09 14:30:22    1065
#> 3 2012-09-10 16:01:20     936
#> 4 2012-07-29 15:20:56     935
#> 5 2014-02-27 10:22:11     755
#> 6 2013-02-20 11:41:07     755
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
#> ⌛ Enriching time: 4.18 secs
#> 
head(df_enriched$data[[1]])
#> # A tibble: 6 x 8
#>   created_at          grid_id  year month   day  wday  hour ymd       
#>   <dttm>                <int> <dbl> <dbl> <int> <dbl> <int> <date>    
#> 1 2014-03-01 11:57:54    1376  2014     3     1     7    11 2014-03-01
#> 2 2012-11-09 14:30:22    1065  2012    11     9     6    14 2012-11-09
#> 3 2012-09-10 16:01:20     936  2012     9    10     2    16 2012-09-10
#> 4 2012-07-29 15:20:56     935  2012     7    29     1    15 2012-07-29
#> 5 2014-02-27 10:22:11     755  2014     2    27     5    10 2014-02-27
#> 6 2013-02-20 11:41:07     755  2013     2    20     4    11 2013-02-20
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
