# Homelocator
Analysis a person's home location based on location and timestamped data
- `homeloc_filter`: Keep only users that meet certain preconditions
- `homeloc_valuate`: Add and valuate new variables derived from timestamp varimultipleable   
- `homeloc_score`: Give a score to each location based on the valuating results of multiple variables
- `homeloc_extract`: Extract the location with the highest score and treat it as the user's home location

## Build package 
``` devtools::build()```

## Install package 
```devtools:install()```

## Load package 
``` Ctrl/Cmd + Shift + L``` or ```devtools::load_all()```

## Sample Testing
```{r}
df <- read_csv(system.file("extdata", "test_sample.csv", package = "homelocator", mustWork = TRUE)) 
df %>% 
  homeloc_filter() %>% 
  homeloc_valuate() %>% 
  homeloc_score() %>% 
  homeloc_extract()
```







