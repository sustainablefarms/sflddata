# Extract Woody Cover
# it is in Albers Tile format with CRS = CRS("+init=epsg:3577").
# (see analysis 1_2_checking_albers_tiles.Rmd for why)
# Tiles are in folders xmin_ymin in Easting and Northings, where xmin and ymin are in '000 000s of meters.

invisible(lapply(c("raster", "maptools", "rgdal", "ncdf4", "lubridate", "parallel"),
       library, character.only = TRUE))
invisible(lapply(paste0("./R/", list.files("./R/")), source))


# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
ptsraw <- sws_sites_2_spdf(sws_sites)
points <- spTransform(sws_sites_2_spdf(sws_sites),
                      CRS("+init=epsg:3577"))
spobj <- buffer(points, 1000) #the buffer here to make sure extracted brick includes extra around the points

#load / read raster values
b <- brick_woodycover(spobj, 2000:2019)
writeRaster(b, "./tmpdata/woodycover_all_tmp.grd", overwrite = TRUE)
b_lowres <- aggregate(b, fact = 2^7)
names(b_lowres) <- 2000:2019
b_lowres <- projectRaster(b_lowres, brick("./private/data/remote_sensed/gpp_mean.grd"),  method = "bilinear")
writeRaster(b_lowres, "./private/data/remote_sensed/woodycover_all_lowres.grd", overwrite = TRUE) #use grd cos layer names are saved

#compute average of buffer for every pixel
wf <- focalWeight(b, 500, type = "circle") 
cl <- makeCluster(3)
bs <- focal_bylayer(b, wf, fun = sum, cl = cl)
stopCluster(cl)
names(bs) <- names(b)
stopifnot(all(minValue(bs) >= 0))
writeRaster(bs, "./private/data/remote_sensed/woodycover_all_500mrad_EN.grd", overwrite = TRUE)


bs_newproj <- projectRaster(bs, brick("./private/data/derived/m1b_resid_Sept6th.grd"),  method = "bilinear")
stopifnot(all(minValue(bs_newproj) >= 0))
writeRaster(bs_newproj, "./private/data/remote_sensed/woodycover_all_500mrad.grd", overwrite = TRUE)

points_LL <- spTransform(points, crs(brick("./private/data/derived/m1b_resid_Sept6th.grd")))
woodycover_500mradius <- t(raster::extract(bs_newproj, points_LL))
colnames(woodycover_500mradius) <- points$SiteCode
years <- year(as_date(rownames(woodycover_500mradius), format =  "X%Y", tz = "Australia/Sydney"))
woodycover_500mradius <- cbind(year = years, data.frame(woodycover_500mradius))
session <- sessionInfo()
save(woodycover_500mradius, session, file = "./private/data/remote_sensed/woodycover_500mradius.Rdata")
