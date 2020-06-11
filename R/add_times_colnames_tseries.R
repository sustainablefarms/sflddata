#' @title Add column names and read row names into times
#' @description Very short function for convenience in converting matrices from \code{t(extract(brick, SpatialPoints))} into a dataframe with a column of time stamps, and column names given by site.
#' @param tseries A matrix or data frame with rownames giving time stamps.
#' @param newcolnames Column names to assign to tseries (not including the new time column)
#' @param format The format to supply to lubridate's as_date function.
add_colnames_times_tseries <- function(tseries, newcolnames, format = "X%Y.%m.%d"){
  colnames(tseries) <- newcolnames
  times <- lubridate::as_date(rownames(tseries), format =  format, tz = "Australia/Sydney")
  tseries <- cbind(times, data.frame(tseries))
  return(tseries)
}