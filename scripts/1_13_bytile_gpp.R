# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf); library(raster);
devtools::load_all()
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

#### read RS rasters ####
gppl <- lapply(1:nrow(locs_wgs84),
      FUN = function(id) {
        roi <- tilearoundobj(locs_wgs84[id, ], 0.01) #0.01 degrees
        gpp <- gpp_vals(roi, locs_wgs84[id, ], 2000:2018)
        return(gpp)
      })
gpp <- do.call(rbind, gppl)
saveRDS(gpp, "./tmpdata/gpp.rds")

#### convert data into nicer format ready to import for modeling ####
gpp$mean <- rowMeans(gpp)
gpp$SiteCode <- locs_wgs84$SiteCode
saveRDS(gpp, "./private/data/remote_sensed/8d_gpp.rds")



# the following suggests a buffer of 0.01 degrees will be plenty for 500m buffers
# st_distance(locs_sf[1, ], locs_sf[2, ])
# locs_sf[c(1, 2), ]
# locs_sf[1, "geometry"] - locs_sf[2, "geometry"] 
# 1 degree of latitude at 40 South is about 111km (https://longitudestore.com/how-big-is-one-gps-degree.html)
# 1 degree of longitude at 40 South is about 85km (https://longitudestore.com/how-big-is-one-gps-degree.html)
# There a generous buffer would be about 0.01 degree in longitude and latitude.

