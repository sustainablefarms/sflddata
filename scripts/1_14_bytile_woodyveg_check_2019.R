
library(sf)

devtools::load_all()
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

nngpts <- locs_wgs84[locs_wgs84$StudyCode == "Nanangroe Natural Experiment", ]

woodyb <- brick_woodycover(sf::as_Spatial(nngpts), 2018:2019)
gppb <- brick_gpp(sf::as_Spatial(nngpts), 2000)
raster::plot(woodyb)
raster::plot(gppb)
