# Home Location 
Calculate a person's home location based on location and timestamped data

For each user:
- filtering step:
    - total_tweets_counts > 20 
    - total_tweets_counts_per_tract > 20
    - study_period_per_tract > 10
    - unique_days_per_tract > 10
    - hours_per_tract > 8
    
- temporal variables:
    - week (weekay & weekend): 20% tweets sent on weekend 
    - daytimes (work time & night time): tweets sent on night time > tweets sent on work time 
    - month (Jan-Dec): tweets sent on more than 6 months 
    - day (Mon-Sun): tweets sent on all day of weeks 
        
        
    


