# Homelocator
## Introduction
User's locations are important to many applications such as event detection, epidemic dispersion, targeted advertisment, news recommendation and so on. In this package, we estimate users' home location with location and timestamped Twitter data at neighborhood-level. Our algorithm analyzes score of severial variables that can represent users' tweetting behavior. Ultimately, we would like to be able to predict the home location of each user based on the score of locations cumulated by variables we considered. 

## Main functions 
- `homeloc_filter`: Keep only users that meet certain preconditions
- `homeloc_valuate`: Add and valuate new variables derived from timestamp varimultipleable   
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
### Based on social network 
- [Shiori Hironaka et al.]: Analyze the function of network-based home location estimation with iteration while using the social network based on following relationship on Twitter
  - **Spatial Label Propagation (SLP)**: a way of applying the home location estimation method using the labels of the adjacent nodes iteratively
  
- [Jinpeng Chen et al.]: Estimate a Twitter user's city-level location based on the user's following network, user-centric data, and tie strength
  - **Social Tie Factor Graph (STFG) model**: In STFG, relationships between users and locations are modeled as nodes, while attributes and correlations are modeled as factors
  
- [Rui Li et al.]: propose a **unified discriminative influence model (UDI)** for profiling users' locations. UDI intergrates signals observed from both social network (friends) and user-centric data (tweets) in a unified probabilistic framework. 


### Based on tweets contents 
- [JALAL MAHMUD et al.]: Inferring the home location of Twitter users at different granularities, including city, state, time zone, or geographic region, using the content of users’ tweets and their tweeting behavior. The algorithm uses an ensemble of statistical and heuristic classifiers to predict locations
  - content-based statistical classifier uses a **Bayesian model** of local word distributions to predict location
  - behavior-based time zone classifier uses **novel temporal behavior-based features** 

- [Hecht et al.2011] built **Bayesian probabilistic models** from words in tweets for estimating the country- and state-level location of Twitter users.

- [Cheng et al.2010] describe a **city-level** location estimation algorithm that is based on identifying local words from tweets and building **statistical predictive models** from them 

- [Chandar et al. 2011] described location estimation using the **conversation relationship** of Twitter users in addition to the text content used in the conversation

- [Chang et al. 2012] described content-based location detection method using **Gaussian Mixture Model (GMM)** and **the Maximum Likelihood Estimation (MLE)**. Their method also eliminates noisy data from tweet content using the notion of nonlocalness and geometric localness.

- [Zhiyuan Cheng et al.]: propose and evaluate a probabilistic framework for estimating a Twitter user’s **city-level** location based purely on the content of the user’s tweets, even in the absence of any other geospatial cues
  - reliance purely on tweet content
  - a classification component for automatically identifying words in tweets with a strong local geo-scope
  - a **lattice-based neighborhood smoothing model** for refining a user's location estimate 
  
- [Eisenstein et al. 2011] built **geographic topic models** to predict the location of Twitter users in terms of regions and states.











