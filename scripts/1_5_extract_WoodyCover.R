# Extract Woody Cover
# it is in Albers Tile format with CRS = CRS("+init=epsg:3577").
# (see analysis 1_2_checking_albers_tiles.Rmd for why)
# Tiles are in folders xmin_ymin in Easting and Northings, where xmin and ymin are in '000 000s of meters.

invisible(lapply(c("raster", "maptools", "rgdal", "ncdf4", "lubridate"),
       library, character.only = TRUE))
invisible(lapply(paste0("./functions/", list.files("./functions/")), source))


# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
ptsraw <- sws_sites_2_spdf(sws_sites)
points <- spTransform(sws_sites_2_spdf(sws_sites),
                      CRS("+init=epsg:3577"))
spobj <- buffer(points, 1000) #the buffer here to make sure extracted brick includes extra around the points

#load / read raster values
b <- brick_woodycover(spobj, 2000:2018)
b_lowres <- aggregate(b, fact = 2^7)
names(b_lowres) <- 2000:2018
b_lowres <- projectRaster(b_lowres, brick("./private/data/remote_sensed/gpp_mean.grd"),  method = "bilinear")
writeRaster(b_lowres, "./private/data/remote_sensed/woodycover_all_lowres.grd") #use grd cos layer names are saved

#compute average of buffer for every pixel
wf <- focalWeight(b, 500, type = "circle") 
bs <- focal_bylayer(b, wf, fun = sum)
names(bs) <- names(b)
bs_newproj <- projectRaster(b, brick("./private/data/derived/m1b_resid_Sept6th.grd"),  method = "bilinear")
bs_newproj <- resample(bs_newproj, brick("./private/data/derived/m1b_resid_Sept6th.grd"))
writeRaster(bs_newproj, "./private/data/remote_sensed/woodycover_all_500mrad.grd", overwrite = TRUE)

woodycover_500mradius <- t(extract(bs, points))
colnames(woodycover_500mradius) <- points$SiteCode
years <- year(as_date(rownames(woodycover_500mradius), format =  "X%Y", tz = "Australia/Sydney"))
woodycover_500mradius <- cbind(year = years, data.frame(woodycover_500mradius))
session <- sessionInfo()
save(woodycover_500mradius, session, file = "./private/data/remote_sensed/woodycover_500mradius.Rdata")
