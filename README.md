# Homelocator
## Introduction
User's locations are important to many applications such as event detection, epidemic dispersion, targeted advertisment, news recommendation and so on. In this package, we estimate users' home location with location and timestamped Twitter data at neighborhood-level. Our algorithm analyzes score of severial variables that can represent users' tweetting behavior. Ultimately, we would like to be able to predict the home location of each user based on the score of locations cumulated by variables we considered. 

## Main functions 
- `homeloc_filter`: Keep only users that meet certain preconditions
- `homeloc_valuate`: Add and valuate new variables derived from timestamp variable   
- `homeloc_score`: Give a weight and score to each location based on the valuating results of multiple variables
- `homeloc_extract`: Extract the location with the highest score and treat it as the user's home location

## Install
### Build package 
``` devtools::build()```

### Install package 
```devtools:install()```

### Load package 
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

## Other algorithms for home location estimation
### Based on geo-tagged data 
- [Jie Lin et al.2016]: Four approaches have been proposed and evaluated to address the problem of automatic identification of a Twitter user’s home location based on the spatiotemporal information of his/her geo-tagged tweets. The three deterministic approaches, WMFV, W-MEAN, and W-MEDIAN, identify the home location of each Twitter user by find- ing the weighted most visited cluster, the weighted mean center of geo-tagged tweets, and the weighted median cen- ter of geo-tagged tweets of that user, respectively. In contrast, SVM formulates the problem as a binary classification task within a machine learning framework. The results show that the WMFV and SVM methods provide more satisfactory performance in predicting home locations compared with the W-MEDIAN and W-MEAN methods.

- [Efstathiades et al.2015]: proposed an approach to infer the precise home location of a Twitter user by taking the most popular location of that user's geo-tagged tweets during the so-called "Rest" and "Leisure" timeframes. Noticing that tweets generated during the Rest timeframe are more likely sent from home than those during the Leisure timeframe, they increased the popularity of a place if the tweet was generated during Rest time by assigning a higher weight value.

- [Zheng et al. 2015]: proposed an approach for predicting a Flickr user’s geo-point home location based on the temporal features and visual features of that user’s photo collections. 

### Based on passive positioning data
- [Rein Ahas et al.]: to develop and to test the model for determining meaningful locations of mobile phone users as locations of homes and work-places using passive mobile positioning data and anchor point model. Passive mobile positioning data is secondary data concerning the location of call activities or handovers in network cells that is automatically stored in the memory of service providers.
- [Pilleriine Kamenjuk et al.]: mapping changes of residence using data from passive mobile positioning and an anchor point model to better understanding long-term mobility. The study concludes that the modst important considerations in monitoring change of residence using passive mobile position data include the continuity of the time-series data, the varing structure of the mobile tower network and the diversified nature of human mobility. 

### Based on social network 
- [Davis et al. 2011]: presented a simple but effective approach for inferring a user's city-level by taking the most-frequently seen location of that user's friends.

- [Jurgens et al.2013]: calculated each user's home location as the geometric median of their friend's locations. To overcome the problem of a user having too few friends with known location information, locations are inferred with an interative procedure which uses the estimated locations as ground truth in the next round of location estimation.

- [Compton et al. 2014]: improved Jurgens’s (2013) method by weighting each user’s friends’ locations based on how many times the user interacted with them and restricting the geographic dispersion of each user’s network during each update to prevent erroneous locations being propagated through subsequent iterations.

- [McGee et al. 2013]: divided a user's relationships into 10 partitions.

- [Shiori Hironaka et al.]: Analyze the function of network-based home location estimation with iteration while using the social network based on following relationship on Twitter
  - **Spatial Label Propagation (SLP)**: a way of applying the home location estimation method using the labels of the adjacent nodes iteratively
  
- [Jinpeng Chen et al.]: Estimate a Twitter user's city-level location based on the user's following network, user-centric data, and tie strength
  - **Social Tie Factor Graph (STFG) model**: In STFG, relationships between users and locations are modeled as nodes, while attributes and correlations are modeled as factors
  
- [Rui Li et al.]: propose a **unified discriminative influence model (UDI)** for profiling users' locations. UDI intergrates signals observed from both social network (friends) and user-centric data (tweets) in a unified probabilistic framework. 


### Based on tweets contents 
- [JALAL MAHMUD et al.]: Inferring the home location of Twitter users at different granularities, including city, state, time zone, or geographic region, using the content of users’ tweets and their tweeting behavior. The algorithm uses an ensemble of statistical and heuristic classifiers to predict locations
  - content-based statistical classifier uses a **Bayesian model** of local word distributions to predict location
  - behavior-based time zone classifier uses **novel temporal behavior-based features** 

- [Cheng et al.2010] describe a **city-level** location estimation algorithm that is based on identifying local words from tweets and building **statistical predictive models** from them 

- [Hecht et al.2011] built **Bayesian probabilistic models** from words in tweets for estimating the country- and state-level location of Twitter users.

- [Kinsella et al.2011]: used language model-based approaches to estimate Twitter users' home locations at the granularity levels of country, state, city, and ZIP code. The prediction accuracy decreased as the granularity of location increased. 

- [Chandar et al. 2011] described location estimation using the **conversation relationship** of Twitter users in addition to the text content used in the conversation

- [Zhiyuan Cheng et al.]: propose and evaluate a probabilistic framework for estimating a Twitter user’s **city-level** location based purely on the content of the user’s tweets, even in the absence of any other geospatial cues
  - reliance purely on tweet content
  - a classification component for automatically identifying words in tweets with a strong local geo-scope
  - a **lattice-based neighborhood smoothing model** for refining a user's location estimate 
  
- [Eisenstein et al. 2011] built **geographic topic models** to predict the location of Twitter users in terms of regions and states.

- [Chang et al. 2012] described content-based location detection method using **Gaussian Mixture Model (GMM)** and **the Maximum Likelihood Estimation (MLE)**. Their method also eliminates noisy data from tweet content using the notion of nonlocalness and geometric localness.

- [Mahmud et al.2014]: proposed a hierarchical location estimator, in which a Twitter user’s time zone, region, or state was determined first by high-level classifier, to predict a city-level location at a lower level. They reported that all three hierarchical classifiers are superior to the single-level one for city prediction, which is due to the fact that fewer cities need to be discriminated at the lower level in their two-level hierarchical system.










