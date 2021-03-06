% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topN.R
\name{top_n_nested}
\alias{top_n_nested}
\title{Select top n rows by certain value}
\usage{
top_n_nested(df, n = 2, wt)
}
\arguments{
\item{df}{A nested dataframe}

\item{n}{Number of rows to return}

\item{wt}{The variable to use for ordering}
}
\description{
Select top n rows in each group, ordered by wt within a nested dataframe
}
