# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf); library(raster)
devtools::load_all()
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

# locs_wgs84 %>%
#   filter(StudyCode == "Restoration Study") %>%
#   st_bbox()
tileswpts <- divide_into_tiles(locs_wgs84, cellsize = 0.05, buffer = 0.01)
tiles <- lapply(tileswpts, function(x) st_as_sf(x$tile))
tilegrd <- do.call(rbind, tiles)
# plot(tilegrd)

length(tileswpts)

# tile <- tileswpts[[1]]$tile
# pts <- tileswpts[[1]]$pts

woodyl <- pbapply::pblapply(1:length(tileswpts),
                 FUN = function(id) {
                   roi <- tilearoundobj(tileswpts[[id]]$tile, 0.01) #0.01 degrees
                   woody <- woody_vals_buffer(roi, tileswpts[[id]]$pts, 2000:2018, 500) #500 metres around each point
                   return(woody)
                 })
woody <- do.call(rbind, woodyl)
# saveRDS(woody, "./tmpdata/woody.rds")

#### convert data into nicer format ready to import for modeling ####
woody$SiteCode <- locs_wgs84$SiteCode
# saveRDS(woody, "./private/data/remote_sensed/woody500.rds")