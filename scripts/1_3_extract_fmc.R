# Extract FMC values
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./R/", list.files("./R/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)

# load raster values
fmc_mean_brick <- brick_fmc(points, 2001:2019)

###### Saving Extracts of Data ####
# fmc_mean time series
fmc_mean <- t(extract(fmc_mean_brick, points))
fmc_mean <- add_colnames_times_tseries(fmc_mean, points$SiteCode)
session <- sessionInfo()
save(fmc_mean, session, file = "./private/data/remote_sensed/fmc_mean.Rdata")

# average fmc_mean across time dimension only:
## note that each pixel has na values. Some regions are completely NA, but this is not where the points are.
# max(colSums(is.na(fmc_mean))) == 29 #out of 1737 layers
# plot(sum(is.na(fmc_mean_brick)))
# plot(add = TRUE, points)

fmc_mean_tmn_ras <- mean(fmc_mean_brick, na.rm = TRUE)
fmc_mean_tmn <- extract(fmc_mean_tmn_ras, points)
names(fmc_mean_tmn) <- points$SiteCode
session <- sessionInfo()
save(fmc_mean_tmn, session, file = "./private/data/remote_sensed/fmc_mean_tmn.Rdata")

# time series of difference to mean
difftotmn <- fmc_mean_brick - fmc_mean_tmn_ras
names(difftotmn) <- names(fmc_mean_brick)
fmc_mean_difftotmn <- t(extract(difftotmn, points))
fmc_mean_difftotmn <- add_colnames_times_tseries(fmc_mean_difftotmn, points$SiteCode)
session <- sessionInfo()
save(fmc_mean_difftotmn, session, file = "./private/data/remote_sensed/fmc_mean_difftotmn.Rdata")
