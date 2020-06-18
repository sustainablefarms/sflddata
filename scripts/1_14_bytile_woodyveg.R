# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf); library(raster)
devtools::load_all()
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

woodyl <- lapply(1:nrow(locs_wgs84),
                 FUN = function(id) {
                   roi <- tilearoundobj(locs_wgs84[id, ], 0.01) #0.01 degrees
                   woody <- woody_vals_buffer(roi, locs_wgs84[id, ], 2000:2018, 500) #500 metres around each point
                   return(woody)
                 })
woody <- do.call(rbind, woodyl)
saveRDS(woody, "./tmpdata/woody.rds")

#### convert data into nicer format ready to import for modeling ####
woody$SiteCode <- locs_wgs84$SiteCode
saveRDS(woody, "./private/data/remote_sensed/woody500.rds")