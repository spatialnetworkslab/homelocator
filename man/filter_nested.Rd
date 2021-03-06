% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/filter.R
\name{filter_nested}
\alias{filter_nested}
\title{Return rows with matching condition within nested dataframe}
\usage{
filter_nested(df, user = "u_id", ...)
}
\arguments{
\item{df}{A nested dataframe}

\item{user}{Name of column that holds unique identifier for each user}

\item{...}{Logical predicates defined in terms of the variables in df. Only rows match conditions are kept.}
}
\description{
Filter finds rows where conditions are true within nested dataframe
}
