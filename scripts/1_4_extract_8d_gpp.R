# Extract time series of daily precipitation
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
source("./functions/extract_ts_wald.R")
source("./functions/sites_2_sp_points.R")

# get spatial points
source("./functions/sites_2_sp_points.R")
sws_sites <- readRDS("./data/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)

#prepare raster extraction
filenametemplate <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc"
years <- 2000:2018
files <- vapply(years, function(x) gsub("2000", as.character(x), filenametemplate), FUN.VALUE = "characterlikethis")
xo <- nc_open(files[[1]])
varname <- "GPP"

tseries.l <- lapply(files, function(x) {
                tseries <- tryCatch(
                  {t(extract_ts_wald(points,
                                     x,
                                     varname = varname))},
                  error = function(cond){
                    message(paste("Error in reading", x))
                    message(cond)
                    return(NULL)
                  })
                tseries <- ##checked on 2019-12-04: this line does not impact result. Line is still here for good recordkeeping
                return(tseries)})
tseries <- do.call(rbind,tseries.l)
tseries <- as.data.frame(tseries)
times <- as_date(rownames(tseries), format =  "X%Y.%m.%d", tz = "Australia/Sydney")
tseries <- cbind(times, tseries)
gpp_8d <- tseries
rm(tseries)
session <- sessionInfo()
save(gpp_8d, session, file = "./data/gpp_8d.Rdata")



