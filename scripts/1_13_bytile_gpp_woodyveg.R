# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf)
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_sf <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_sf <- st_set_crs(locs_sf, 4326) #4326 is the epsg code for WGS84

# the following suggests a buffer of 0.01 degrees will be plenty for 500m buffers
st_distance(locs_sf[1, ], locs_sf[2, ])
locs_sf[c(1, 2), ]
locs_sf[1, "geometry"] - locs_sf[2, "geometry"] 
# 1 degree of latitude at 40 South is about 111km (https://longitudestore.com/how-big-is-one-gps-degree.html)
# 1 degree of longitude at 40 South is about 85km (https://longitudestore.com/how-big-is-one-gps-degree.html)
# There a generous buffer would be about 0.01 degree in longitude and latitude.

tileswpts <- divide_into_tiles(locs_sf, cellsize = 1, buffer = 0.01)
tiles <- do.call(rbind, lapply(tileswpts, function(x) st_as_sf(x$tile)))
pts_l <- lapply(1:length(tileswpts), function(x) cbind(tile = x, st_as_sf(tileswpts[[x]]$pts)))
pts <- do.call(rbind, pts_l)
library(ggplot2)
ggplot() +
  geom_sf(data = tiles) +
  geom_sf(aes(col = tile), data = pts[, "tile"]) +
  scale_color_viridis_c()
rbind(st_as_sf(tileswpts[[1]]$tile), st_as_sf(tileswpts[[2]]$tile))
