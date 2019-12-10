# Extract GPP values 
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
roi <- extent(points)


#prepare raster file names
files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
                             "8day/GPP",
                             "OzWALD",
                             "GPP",
                             2000:2018,
                             "nc")

#extract raster data given prior knowledge of format of the netCDF files
gpp_8d_sws_brick <- extract_brick_files(files, "GPP", roi, dims = c(2, 1, 3),
                                        timeconvertfun = function(t) as_date("1800-01-01") + days(t))
#this data is not in memory so won't save


###### Saving Extracts of Data ####
# gpp time series
gpp_8d <- t(extract(gpp_8d_sws_brick, points))
gpp_8d <- add_colnames_times_tseries(gpp_8d, points$SiteCode)
session <- sessionInfo()
save(gpp_8d, session, file = "./private/data/gpp_8d.Rdata")

# average gpp across time dimension only:
gpp_8d_tmn_ras <- mean(gpp_8d_sws_brick)
gpp_8d_tmn <- extract(gpp_8d_tmn_ras, points)
names(gpp_8d_tmn) <- points$SiteCode
session <- sessionInfo()
save(gpp_8d_tmn, session, file = "./private/data/gpp_8d_tmn.Rdata")

# time series of difference to mean
difftotmn <- gpp_8d_sws_brick - gpp_8d_tmn_ras
names(difftotmn) <- names(gpp_8d_sws_brick)
gpp_8d_difftotmn <- t(extract(difftotmn, points))
gpp_8d_difftotmn <- add_colnames_times_tseries(gpp_8d_difftotmn, points$SiteCode)
session <- sessionInfo()
save(gpp_8d_difftotmn, session, file = "./private/data/gpp_8d_difftotmn.Rdata")
