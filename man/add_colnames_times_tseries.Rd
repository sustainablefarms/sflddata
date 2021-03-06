% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_times_colnames_tseries.R
\name{add_colnames_times_tseries}
\alias{add_colnames_times_tseries}
\title{Add column names and read row names into times}
\usage{
add_colnames_times_tseries(tseries, newcolnames, format = "X\%Y.\%m.\%d")
}
\arguments{
\item{tseries}{A matrix or data frame with rownames giving time stamps.}

\item{newcolnames}{Column names to assign to tseries (not including the new time column)}

\item{format}{The format to supply to lubridate's as_date function.}
}
\description{
Very short function for convenience in converting matrices from \code{t(extract(brick, SpatialPoints))} into a dataframe with a column of time stamps, and column names given by site.
}
