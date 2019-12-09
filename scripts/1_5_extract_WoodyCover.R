# Extract Woody Cover
# it is in Albers Tile format.
# These are in folders xmin_ymin in Easting and Northings
# according to https://spatialreference.org/ref/epsg/3577/: CRS("+init=epsg:3577") is GDA94 / Australia Albers. It matches the crs pulled from the netCDF files

invisible(lapply(c("raster", "maptools", "rgdal", "ncdf4", "lubridate"),
       library, character.only = TRUE))
invisible(lapply(paste0("../linking-data/functions/", list.files("../linking-data/functions/")), source))


# Construct Region Desired
sws_sites <- readRDS("../linking-data/data/sws_sites.rds")
points <- spTransform(sws_sites_2_spdf(sws_sites), 
                      CRS("+proj=aea +lat_1=-18 +lon_0=132 +lat_0=0 +x_0=0 +y_0=0 +a=6378137 +rf=298.257222101 +lat_2=45.5"))
roi <- extent(points)

# Bricks for each tile that overlays the sws points:
files1 <- build_filename_list(
  "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover",
  "DEA_ALC/13_-41",
  "fc_metrics_13_-41_",
  2000:2018,
  ".nc",
  namesep = "")
source("./functions/extract_brick_files.R")
b1 <- extract_brick_files(files1[1], varname = "WCF", roi, dims = c(2, 1), timeconvertfun = function(t) t)



b1 <- raster("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC/16_-39/fc_metrics_16_-39_2000.nc", varname = "WCF", dims = 2:1)
b2 <- raster("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC/17_-39/fc_metrics_17_-39_2000.nc", varname = "WCF", dims = 2:1)
b3 <- raster("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC/18_-39/fc_metrics_18_-39_2000.nc", varname = "WCF", dims = 2:1)
b4 <- raster("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC/14_-39/fc_metrics_14_-39_2000.nc", varname = "WCF", dims = 2:1)
b.l <- list(b1, b2, b3, b4)
m <- do.call(mosaic, c(b.l, fun = "mean"))

# Check that points and extents overlap:
nrow(intersect(points, extent(b1))) > 0


#

crop(b1, roi, snap = "out")

extract(m, points)

plot(b2)
plot(add = TRUE, points)
plot(points)
plot(add = TRUE, extent(b1))
plot(add = TRUE, extent(b2))
plot(add = TRUE, extent(b3))
plot(add = TRUE, extent(b4))
