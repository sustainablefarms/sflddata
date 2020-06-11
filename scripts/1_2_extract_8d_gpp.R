# Extract GPP values 
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./R/", list.files("./R/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
roi <- extent(points)

#build brick using function
gpp_8d_sws_brick <- brick_gpp(points, 2000:2018)

#extract layers on September 6th
gpp_brick_sept <- gpp_8d_sws_brick[[grep("X.....09.06", names(gpp_8d_sws_brick))]]
writeRaster(gpp_brick_sept, filename = "./private/data/remote_sensed/gpp_Sept6th.grd", overwrite = TRUE)

# average gpp across time dimension only:
gpp_8d_tmn_ras <- mean(gpp_8d_sws_brick)
writeRaster(gpp_8d_tmn_ras, filename = "./private/data/remote_sensed/gpp_mean.grd", overwrite = TRUE)

# time series of difference to mean
difftotmn <- gpp_8d_sws_brick - gpp_8d_tmn_ras
names(difftotmn) <- names(gpp_8d_sws_brick)
# write out only Sept 6th:
writeRaster(difftotmn[[grep("X.....09.06", names(difftotmn))]],
            filename = "./private/data/remote_sensed/gpp_diff_to_mean_Sept6th.grd", overwrite = TRUE)


###### Saving Point Extracts of Data ####
# gpp time series
gpp_8d <- t(extract(gpp_8d_sws_brick, points))
gpp_8d <- add_colnames_times_tseries(gpp_8d, points$SiteCode)
session <- sessionInfo()
save(gpp_8d, session, file = "./private/data/remote_sensed/gpp_8d.Rdata")

# average gpp across time dimension only:
gpp_8d_tmn <- extract(gpp_8d_tmn_ras, points)
names(gpp_8d_tmn) <- points$SiteCode
session <- sessionInfo()
save(gpp_8d_tmn, session, file = "./private/data/remote_sensed/gpp_8d_tmn.Rdata")

# time series of difference to mean
gpp_8d_difftotmn <- t(extract(difftotmn, points))
gpp_8d_difftotmn <- add_colnames_times_tseries(gpp_8d_difftotmn, points$SiteCode)
session <- sessionInfo()
save(gpp_8d_difftotmn, session, file = "./private/data/remote_sensed/gpp_8d_difftotmn.Rdata")
