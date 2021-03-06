% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/identify_location.R
\name{recipe_OSNA}
\alias{recipe_OSNA}
\title{recipe: Online Social Networks Activity - OSNA}
\usage{
recipe_OSNA(
  df,
  user = "u_id",
  timestamp = "created_at",
  location = "loc_id",
  show_n_loc,
  keep_score = F
)
}
\arguments{
\item{df}{An enriched dataframe}

\item{user}{Name of column that holds unique identifier for each user}

\item{timestamp}{Name of timestamp column. Should be POSIXct}

\item{location}{Name of column that holds unique identifier for each location}

\item{show_n_loc}{Number of potential homes to extract}

\item{keep_score}{Option to keep or remove calculated result/score per user per location}
}
\description{
recipe: Online Social Networks Activity - OSNA
}
