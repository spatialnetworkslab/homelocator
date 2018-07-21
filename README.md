# Home Location 
Calculate a person's home location based on location and timestamped data

For each user:
- filtering:
    - total_tweets_counts > 20 
    - total_tweets_counts_per_tract > 20
    - study_period_per_tract > 10
    - unique_days_per_tract > 10
    - unique_hours_per_tract > 8
    
- temporal variables:
    - week (weekay & weekend): drop tract which only has tweets sent on weekday & calcualte the percentage of tweets sent on weekend
    - daytimes (work time & night time): drop tract which only has tweets sent on work time & keep the percentage of tweets sent on night time
    - month (Jan-Dec): calculate the distinct months that has tweets 
    - day (Mon-Sun): calculate the distinct day of week that has tweets 

- weighting: 
    - combine all info for each user, the info includes:
    ```u_id, GEOID, total_counts, count_tract, study_period, unique_days, months, days, hours,percent_weekend, percent_Sat_morning, percent_nighttime, counts_group```
     - give a weight to each variable and get a final score
     - change weight and monitor the change 
        
        
    


