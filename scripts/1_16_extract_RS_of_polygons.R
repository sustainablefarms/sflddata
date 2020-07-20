# tile region into 1 degree x 1 degree pieces and apply existing scripts
library(sf); library(raster); library(sustfarmld)
polys <- readRDS("../farm_biodiversity_app/app/data/sa2_polygons.rds")
polys <- tibble::rowid_to_column(polys, var = "idx")
# plot(polys)

woodyl <- lapply(polys$idx, function(x) return(NULL))
# woodyl <- readRDS("./tmpdata/woodyl.rds")
uncompleted <- vapply(woodyl, is.null, FUN.VALUE = FALSE)
attempts <- 0

rasterOptions(tmpdir = "/media/kassel/Seagate1TB/tmpdir/")
while(any(uncompleted) && attempts <= 6){
  cat(sum(uncompleted), "polygons remain.\n")
  woodyl[uncompleted] <- pbapply::pblapply(polys$idx[uncompleted],
                  function(i){
                    poly <- as(polys[i, ], "Spatial")
                    b <- bmask <- NULL
                    try({
                      b <- brick_woodycover(poly, 2000:2019)
                      poly3577 <- spTransform(poly, CRS(proj4string(b)))
                      bmask <- raster::mask(b, poly3577)
                    })
                    gc()
                    return(bmask)
                  })
  uncompleted <- vapply(woodyl, is.null, FUN.VALUE = FALSE)
  attempts <- attempts + 1
}
saveRDS(woodyl, "./tmpdata/woodyl.rds")
mapply(raster::writeRaster, x = woodyl[!uncompleted], filename = paste0("/media/kassel/Seagate1TB/tmpdir/poly_",polys$idx[!uncompleted], ".grd"))

#### Preparing Smoothed Versions ####
rasterOptions(tmpdir = "/media/kassel/Seagate1TB/tmpdir/")
woodyl <- lapply(paste0("/media/kassel/Seagate1TB/tmpdir/poly_",polys$idx, ".grd"), brick)
wf <- focalWeight(woodyl[[1]][[1]], d = 500, type = "circle") # produces a warnings, but the matrix output still looks good

inweight <- max(wf)
sumfun <- function(x, na.rm = FALSE){
  near157 <- ((abs(x - inweight * 157) < 1E-6) | (abs(x - 157) < 1E-6) )
  x[near157] <- 0
  if (na.rm){x[is.na(x)] <- 0}
  return(sum(x))
}


# b <- woodyl[[1]][[1]]
# bs <- focal_bylayer(woodyl[[1]], w = wf, fun = sumfun)
woodysl <- pbapply::pblapply(woodyl, focal_bylayer, w = wf, fun = sumfun)
mapply(raster::writeRaster, x = woodysl, filename = paste0("/media/kassel/Seagate1TB/tmpdir/poly_",polys$idx[!uncompleted], "_smooth.grd"))

# plot(woodyl[[1]], zlim = c(0, 100))
warning("woody veg values contain 157 for NA, be sure not to exclude them in any averaging.")
# 
##### Checks ####
library(leaflet)
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326)


leaflet(as(st_transform(polys, 4326), "Spatial")) %>%
  addTiles() %>%
  # addProviderTiles("Esri.WorldImagery") %>%
  addPolygons(label = paste(polys[, "idx", drop = TRUE], polys[, "SA2_NAME16", drop = TRUE])) %>%
  addMarkers(data = as(locs_wgs84, "Spatial"))
