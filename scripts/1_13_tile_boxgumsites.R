# tile region into 1 degree x 1 degree pieces and apply existing scripts

library(sf); require(lwgeom);
library(dplyr); library(tibble);
#' @param cellsize is the width and height (in the same units as point's crs) of the tiles to create
#' @param points is an sf object (rather like a dataframe) of point geometries
#' @param buffer is the spatial distance required around each point
#' (each point must have a tile that contains buffer distance region around it)
divide_into_tiles <- function(points, cellsize = 1, buffer = 0.01){
  buffered <- st_buffer(points, buffer)
  grd <- make_grid_buffer(points, cellsize = cellsize, overlap = 2.01 * buffer)
  #2 * buffer overlap is needed for a point landing directly in middle of overlap. It is overlap / 2 from the edge of each tile
  
  #work out which buffered objects are covered by which tiles
  coverobj <- st_covered_by(buffered, sparse = TRUE, grd)
  iscovered <- (vapply(coverobj, length, FUN.VALUE = 2) > 0)
  stopifnot(all(iscovered))
  # take only the first tile that covers each object
  tileid <- vapply(coverobj, function(x) x[[1]], FUN.VALUE = 2)
  
  # split points into each tile
  ptsfortile <- split(points, factor(tileid))
  tilewpoints <- lapply(names(ptsfortile), function(tileid) {
              tile <- grd[as.numeric(tileid), ]
              pts <- ptsfortile[[tileid]]
              return(list(tile = tile, pts = pts))
            })
  return(tilewpoints)
}

#' @param sfobj An sf object to cover with a grid. Could also be a bbox object.
#' @param cellsize is the size of a single cell
#' @param overlap is the amount of overlap of each cell
#' @value An sf multipolygon object made of squares that overlap by buffer, and cover the region bbox expanded by buffer.
make_grid_buffer <- function(sfobj, cellsize = 0.1, overlap = 0.01){
  # cell left hand edges are actually cellsize - buffer apart
  bbox <- st_bbox(sfobj)
  xmins <- seq(bbox$xmin - overlap, bbox$xmax + overlap, by = cellsize - overlap)
  ymins <- seq(bbox$ymin - overlap, bbox$ymax + overlap, by = cellsize - overlap)
  xmaxs <- xmins + cellsize
  ymaxs <- ymins + cellsize
  mins <- expand.grid(xmins, ymins)
  maxs <- mins + Rfast::rep_row(matrix(cellsize, nrow = 1, ncol = 2), nrow(mins))
  rects <- mapply(make_rectangle, xmin = mins[, 1], ymin = mins[, 2],
                                   xmax = maxs[, 1], ymax = maxs[, 2], SIMPLIFY = FALSE)
  gridman <- st_sfc(rects, crs = st_crs(sfobj)$proj4string)
  return(gridman)
}

make_rectangle <- function(xmin, ymin, xmax, ymax){
  bl <- c(xmin, ymin)
  br <- c(xmax, ymin)
  tr <- c(xmax, ymax)
  tl <- c(xmin, ymax)
  mat <- rbind(bl, br, tr, tl, bl)
  out <- st_polygon(list(mat))
  return(out)
}




ggplot(locs_sf) +
  geom_sf(data = grid, col = "red") +
  geom_sf()

names(locs)

library(leaflet)
map <- leaflet() %>%
  addTiles() %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addScaleBar()
map <- map %>%
  addCircleMarkers(lng = #### Add site means ####
                   locs$MeanLON, lat = locs$MeanLAT,
                   label = paste(locs$SiteCode, locs$Study),
                   col = "red")
print(map)


library(sf)
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_sf <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_sf <- st_set_crs(locs_sf, 4326) #4326 is the epsg code for WGS84

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
