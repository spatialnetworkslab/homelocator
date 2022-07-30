
# homelocator

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/spatialnetworkslab/homelocator/master?urlpath=rstudio)

Paper:

> Chen, Q. and Poorthuis, A. (2021) *Identifying home locations in human mobility data: 
> an open-source R package for comparison and reproducibility*. International Journal of 
> Geographical Information Science, Pages 1425-1448, 
> <https://doi.org/10.1080/13658816.2021.1887489>


## Overview

<div style="text-align: justify">
The goal of `homelocator` is to improve the consistency and replicability of algorithms used for identifying home locations in human mobility data. The easy-to-use `homelocator` package provide a consistent framework and interface for the adoption of different algorithms for location inference. With the package, you are able to write structured, algorithmic ‘recipes’ to identify home locations according to your research requirements. The package also has a number of built-in ‘recipes’ that have been translated from approaches in the existing literature.
</div>

## Installation

``` r
# Install development version from GitHub
install_github("spatialnetworkslab/homelocator")
```

## Example

These are some basic examples that show you how to use common functions
in the package.

### Validate input dataset

You need to make sure the input dataset includes three essential
attributes:

  - a unique identifier for the person or user
  - a unique identifier for the spatial location for the data point
  - a timestamp that reflects the time the data point was created

You can use `validate_dataset()` to validate your input dataset before starting identifying meaningful locations. In this function, you need to specify the names of three essential attribute that used in your dataset.

``` r
# Load homelocator library
library(homelocator)
#> Welcome to homelocator package!
```

``` r
# Load other needed libraries
library(tidyverse)
library(here)
```

``` r
# load test sample dataset 
data("test_sample", package = "homelocator")
df_validated <- validate_dataset(test_sample, user = "u_id", timestamp = "created_at", location = "grid_id")
#> 🎉 Congratulations!! Your dataset has passed validation.
#> 👤 There are 100 unique users in your dataset.
#> 🌏 Now start your journey identifying their meaningful location(s)!
#> 👏 Good luck!
#> 
head(df_validated)
#> # A tibble: 6 x 3
#>       u_id grid_id created_at         
#>      <int>   <int> <dttm>             
#> 1 92298565    1581 2016-04-17 22:43:06
#> 2 33908340    1461 2014-10-03 16:29:48
#> 3 92298565    1136 2014-02-07 07:26:15
#> 4 11616678    1375 2014-07-18 10:08:21
#> 5 11616678    1375 2013-11-24 23:16:24
#> 6 47727539     736 2016-06-21 15:59:49
```

### Nesting users for parallel computing

To speed up computing progress, you can nest the validated dataset by user so that the subsequent location inference can be applied to each
user at the same time.

``` r
df_nested <- nest_verbose(df_validated, c("created_at", "grid_id"))
#> 🛠 Start nesting...
#> ✅ Finish nesting!
#> ⌛ Nesting time: 0.192 secs
#> 
head(df_nested)
#> # A tibble: 6 x 2
#>       u_id data                
#>      <int> <list>              
#> 1 92298565 <tibble [1,291 × 2]>
#> 2 33908340 <tibble [1,170 × 2]>
#> 3 11616678 <tibble [938 × 2]>  
#> 4 47727539 <tibble [307 × 2]>  
#> 5 54875363 <tibble [903 × 2]>  
#> 6 40153763 <tibble [1,688 × 2]>
head(df_nested$data[[1]])
#> # A tibble: 6 x 2
#>   created_at          grid_id
#>   <dttm>                <int>
#> 1 2016-04-17 22:43:06    1581
#> 2 2014-02-07 07:26:15    1136
#> 3 2012-08-18 19:26:31    1038
#> 4 2014-11-25 19:39:00    1699
#> 5 2014-12-23 01:54:55    1499
#> 6 2015-06-01 19:48:18     740
```

### Enrich variables from timestamp

Add additional needed varialbes derived from the timestamp column. These are often used/needed as intermediate variables in home location algorithms, such as year, month, day, day of the week and hour of the day, etc.

``` r
df_enriched <- enrich_timestamp(df_nested, timestamp = "created_at")
#> 🛠 Enriching variables from timestamp...
#> 
#> ✅ Finish enriching! New added variables: year, month, day, wday, hour, ymd.
#> ⌛ Enriching time: 0.708 secs
#> 
head(df_enriched$data[[1]])
#> # A tibble: 6 x 8
#>   created_at          grid_id  year month   day  wday  hour ymd       
#>   <dttm>                <int> <dbl> <dbl> <int> <dbl> <int> <date>    
#> 1 2016-04-17 22:43:06    1581  2016     4    17     1    22 2016-04-17
#> 2 2014-02-07 07:26:15    1136  2014     2     7     6     7 2014-02-07
#> 3 2012-08-18 19:26:31    1038  2012     8    18     7    19 2012-08-18
#> 4 2014-11-25 19:39:00    1699  2014    11    25     3    19 2014-11-25
#> 5 2014-12-23 01:54:55    1499  2014    12    23     3     1 2014-12-23
#> 6 2015-06-01 19:48:18     740  2015     6     1     2    19 2015-06-01
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
    al.2015](https://doi.org/10.1080/10630731003597306)
      - Finds the most ‘popular’ location during ‘rest’, ‘active’ and
        ‘leisure time. Here we focus on ’rest’ and ‘leisure’ time to
        find the most possible home location for each user.
  - `APDM`: [Ahas et
    al. 2010](https://doi.org/10.1080/10630731003597306)
      - Calculates the average and standard deviation of start time data
        points by a single user, in a single location.

#### HMLC

``` r
# default recipe: homelocator -- HMLC
identify_location(test_sample, user = "u_id", timestamp = "created_at", location = "grid_id", show_n_loc = 1, recipe = "HMLC")
```

#### FREQ

``` r
# recipe: Frequency -- FREQ
identify_location(test_sample, user = "u_id", timestamp = "created_at", location = "grid_id", 
                  show_n_loc = 1, recipe = "FREQ")
```

#### OSNA

``` r
# recipe: Online Social Network Activity -- OSNA
identify_location(test_sample, user = "u_id", timestamp = "created_at", location = "grid_id", 
                  show_n_loc = 1, recipe = "OSNA")
```

#### APDM

``` r
# recipe: Online Social Network Activity -- APDM
## APDM recipe strictly returns the most likely home location
## It is important to create your location neighbors table before you use the recipe!!
## example: st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")
##          neighbors <- st_queen(df_sf) ===> convert result to dataframe 
data("df_neighbors", package = "homelocator")
identify_location(test_sample, user = "u_id", timestamp = "created_at", location = "grid_id", 
                  show_n_loc = 1, recipe = "APDM")
```
