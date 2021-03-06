% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_clean_functions.R
\name{extract_data}
\alias{extract_data}
\title{Extracts and cleans data from the ATP URLs}
\usage{
extract_data(x)
}
\arguments{
\item{x}{A vector of ATP URLs}
}
\value{
A dataframe whose rows are players and whose columns are the player
  statistics of interest
}
\description{
With the ATP URLs, this function retreives the desired information. If there
is no incomplete information, the data cleaning procedure will return a
dataset with the players' statistics. If there is incomplete informaiton on
the website, the players' statistics will be reported as "unknown."
}
\seealso{
Other web scraping functions: \code{\link{clean_data}},
  \code{\link{get_ATP_code}}, \code{\link{get_ATP_url}},
  \code{\link{get_plus_name}},
  \code{\link{manipulate_data}}
}
\concept{web scraping functions}
