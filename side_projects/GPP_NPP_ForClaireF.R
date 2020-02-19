# Extracting Net Primary Productinity (NPP) from NASA
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

# Construct Region Desired
sites <- readr::read_csv("./tmpdata/ExclosuresSitesWithBioClimElevation_Filtered_5.2.2020.csv", col_names = TRUE)
points <- SpatialPointsDataFrame(sites[, c("Long", "Lat")], sites, proj4string= sp::CRS("+proj=longlat +datum=WGS84"))
roi <- extent(points)

# open gpp files
gpp_8d_brick <- brick_gpp(points, 2000:2018)

# average across time
gpp_8d_tmn <- mean(gpp_8d_brick)

# extact point values
gpp_8d_tmn_pts <- extract(gpp_8d_tmn, points)

# save mean GPP data
sites$meanGPP <- gpp_8d_tmn_pts
readr::write_csv(sites[, c("ID", "Study_name", "meanGPP")], path = "./tmpdata/ExclosuresSite_mnGPP.csv")


## Getting units
library(ncdf4)
f <- nc_open("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc")
ncatt_get(f, "GPP")$units
# "g m-2 d-1"