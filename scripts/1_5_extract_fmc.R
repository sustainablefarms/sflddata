# Extract FMC values
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

# Construct Region Desired
sws_sites <- readRDS("./data/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
roi <- extent(points)

files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au",
                    "FMC/c6/mosaics",
                    "fmc_c6_",
                    2001:2019,
                    type_extension = ".nc",
                    namesep = "")

#extract raster data given prior knowledge of format of the netCDF files
fmc_mean_brick <- extract_brick_files(files, "fmc_mean", roi, dims = 1:3,
                    timeconvertfun = function(t) as_date(as.POSIXlt(t, origin = lubridate::origin)))

###### Saving Extracts of Data ####
# fmc_mean time series
fmc_mean <- t(extract(fmc_mean_brick, points))
fmc_mean <- add_colnames_times_tseries(fmc_mean, points$SiteCode)
session <- sessionInfo()
save(fmc_mean, session, file = "./data/fmc_mean.Rdata")

# average fmc_mean across time dimension only:
## note that each pixel has na values. Some regions are completely NA, but this is not where the points are.
# max(colSums(is.na(fmc_mean))) == 29 #out of 1737 layers
# plot(sum(is.na(fmc_mean_brick)))
# plot(add = TRUE, points)

fmc_mean_tmn_ras <- mean(fmc_mean_brick, na.rm = TRUE)
fmc_mean_tmn <- extract(fmc_mean_tmn_ras, points)
names(fmc_mean_tmn) <- points$SiteCode
session <- sessionInfo()
save(fmc_mean_tmn, session, file = "./data/fmc_mean_tmn.Rdata")

# time series of difference to mean
difftotmn <- fmc_mean_brick - fmc_mean_tmn_ras
names(difftotmn) <- names(fmc_mean_brick)
fmc_mean_difftotmn <- t(extract(difftotmn, points))
fmc_mean_difftotmn <- add_colnames_times_tseries(fmc_mean_difftotmn, points$SiteCode)
session <- sessionInfo()
save(fmc_mean_difftotmn, session, file = "./data/fmc_mean_difftotmn.Rdata")
