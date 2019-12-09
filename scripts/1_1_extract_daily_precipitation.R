# Extract time series of daily precipitation
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
source("./functions/extract_ts_wald.R")
source("./functions/sites_2_sp_points.R")

# get spatial points
source("./functions/sites_2_sp_points.R")
sws_sites <- readRDS("./data/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)

#prepare raster extraction
filenametemplate <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/daily/OzWALD.daily.Pg.2000.nc"
years <- 2000:2018
files <- vapply(years, function(x) gsub("2000", as.character(x), filenametemplate), FUN.VALUE = "characterlikethis")
#xo <- nc_open(files[[1]])
varname <- "Pg"

tseries.l <- lapply(files, function(x) {
                tseries <- t(extract_ts_wald(points,
                    x,
                    varname = varname))
                return(tseries)})
tseries <- do.call(rbind,tseries.l)
tseries <- as.data.frame(tseries)
times <- as_date(rownames(tseries), format =  "X%Y.%m.%d", tz = "Australia/Sydney")
tseries <- cbind(times, tseries)
pg_daily <- tseries
rm(tseries)
session <- sessionInfo()
save(pg_daily, session, file = "./data/pg_daily.Rdata")



