# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf); library(raster);
devtools::load_all()
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

tileswpts <- divide_into_tiles(locs_wgs84, cellsize = 0.05, buffer = 0.01)  #0.01 degrees
# saveRDS("./tmpdata/gppl_tileswpts.rds")
# tileswpts <- readRDS("./tmpdata/gppl_tileswpts.rds")

#### read RS rasters ####
# empty gppl object
gppl <- lapply(tileswpts, function(x) return(NULL))
# gppl <- readRDS("./tmpdata/gppl.rds")
uncompleted <- vapply(gppl, is.null, FUN.VALUE = FALSE)
attempts <- 0
rasterOptions(tmpdir = "/media/kassel/Seagate1TB/tmpdir2/")
while(any(uncompleted) && attempts <= 5){
  cat("Attempting:", sum(uncompleted), "tiles.\n")
  gppl[uncompleted] <- pbapply::pblapply(tileswpts[uncompleted],
                    FUN = function(x) {
                      gpp <- NULL
                      try(gpp <- gpp_vals(x$tile, x$pts, 2000:2019))
                      gc()
                      return(gpp)
                    })
  uncompleted <- vapply(gppl, is.null, FUN.VALUE = FALSE)
  attempts <- attempts + 1
}

saveRDS(gppl, "./tmpdata/gppl.rds")
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

