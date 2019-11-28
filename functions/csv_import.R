# set some functions to cleanly import remote sensing (rs) data from .csv

library(lubridate) # required

# get farm locations including site name, lat and long
# this currently isn't used because we have this info in other datasets;
# but is retained here in case it comes in handy later
get_locations <- function(
  x # must be a csv
){
  data_raw <- read.table(
    x,
    stringsAsFactors = FALSE,
    skip = 5,
    nrows = 2,
    sep = ",",
    header = TRUE
  )[, -c(1:4)]

  data_clean <- data.frame(
    site = colnames(data_raw),
    latitude = as.numeric(data_raw[1, ]),
    longitude = as.numeric(data_raw[2, ]),
    stringsAsFactors = FALSE
  )

  return(data_clean)
}


# import remote sensing data from those locations
get_observations <- function(x){
  if (.Platform$OS.type == "unix") {skipnum <- 13}
  else {skipnum <- 5}
  data_raw <- read.table(
    x,
    stringsAsFactors = FALSE,
    skip = skipnum,
    sep = ",",
    header = TRUE
  )[-c(1:3), -4]
  colnames(data_raw)[1:3] <- c("year", "month", "day")

  # sort out dates
  data_raw <- data_raw[
      !apply(data_raw[, 1:3], 1, function(a){all(as.character(a) == "")}),
    ]
  data_raw$date <- lubridate::ymd(
    apply(data_raw[, 1:3], 1,
      function(a){paste(as.character(a), collapse = "-")}
    )
  )
  data_raw <- data_raw[, c(ncol(data_raw), 1:(ncol(data_raw)-1))]

  data_long <- data.frame(
    site = rep(colnames(data_raw)[-c(1:4)], each = nrow(data_raw)),
    date = rep(data_raw$date, ncol(data_raw)-4),
    value = do.call(c, data_raw[, -c(1:4)]),
    stringsAsFactors = FALSE
  )

  return(data_long)
}