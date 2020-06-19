# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf); library(raster)
devtools::load_all()
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

# locs_wgs84 %>%
#   filter(StudyCode == "Restoration Study") %>%
#   st_bbox()
# tileswpts <- divide_into_tiles(locs_wgs84, cellsize = 0.05, buffer = 0.01)
tileswpts <- readRDS("./tmpdata/woodyl_tileswpts.rds")
tiles <- lapply(tileswpts, function(x) st_as_sf(x$tile))
tilegrd <- do.call(rbind, tiles)
# plot(tilegrd)

#### read RS rasters ####
# empty gppl object
woodyl <- lapply(tileswpts, function(x) return(NULL))
# woodyl <- readRDS("./tmpdata/woodyl.rds")
uncompleted <- vapply(woodyl, is.null, FUN.VALUE = FALSE)
attempts <- 0

rasterOptions(tmpdir = "/media/kassel/Seagate1TB/tmpdir/")
while(any(uncompleted) && attempts <= 5){
  cat("Attempting:", sum(uncompleted), "tiles.\n")
  woodyl[uncompleted] <- pbapply::pblapply(tileswpts[uncompleted],
                                         FUN = function(x) {
                                           woody <- NULL
                                           try(woody <- woody_vals_buffer(x$tile, x$pts, 2000:2018, 500))
                                           gc()
                                           return(woody)
                                         })
  uncompleted <- vapply(woodyl, is.null, FUN.VALUE = FALSE)
  attempts <- attempts + 1
}

# tile <- tileswpts[[1]]$tile
# pts <- tileswpts[[1]]$pts

saveRDS(woodyl, "./tmpdata/woodyl.rds")
# the version run (commit: fc67eaeae1af096ed56a3a5efbebab5a0e6b26e5) had an issue that woody_vals_buffer did not iterate through the points,
# but repeated extraction for *all* points for every point available. The below confirms this:
all(vapply(tileswpts, function(x) nrow(x$pts)^2, FUN.VALUE = 3) == vapply(woodyl, nrow, FUN.VALUE = 3))

# therefore just take the first pts rows of each output
woodyl_thinned <- lapply(1:length(woodyl), function(id) {
  npts <- nrow(tileswpts[[id]]$pts)
  woodyvals <- woodyl[[id]][1:npts, ]
  woodyvals$SiteCode <- tileswpts[[id]]$pts$SiteCode
  return(woodyvals)
})


woody <- do.call(rbind, woodyl_thinned)
stopifnot(anyDuplicated(woody$SiteCode) == 0)
nrow(woody) == nrow(locs_wgs84)
saveRDS(woody, "./tmpdata/woody.rds")

#### convert data into nicer format ready to import for modeling ####
woody$SiteCode <- locs_wgs84$SiteCode
saveRDS(woody, "./private/data/remote_sensed/woody500.rds")
