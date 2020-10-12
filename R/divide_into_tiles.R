#' @title Divide points into tiles

#' @param cellsize is the width and height (in the same units as point's crs) of the tiles to create
#' @param points is an sf object (rather like a dataframe) of point geometries
#' @param buffer is the spatial distance required around each point
#' (each point must have a tile that contains buffer distance region around it)
#' @return  A list. Each element contains a slot 'tile' which is an sfc_POLYGON object,
#' and a slot 'pts', which is a an sf object of points covered by the tile with sufficient buffer
#' @export
divide_into_tiles <- function(points, cellsize = 1, buffer = 0.01){
  buffered <- sf::st_buffer(points, buffer)
  grd <- make_grid_buffer(points, cellsize = cellsize, overlap = 2.01 * buffer)
  #2 * buffer overlap is needed for a point landing directly in middle of overlap. It is overlap / 2 from the edge of each tile
  
  #work out which buffered objects are covered by which tiles
  coverobj <- sf::st_covered_by(buffered, sparse = TRUE, grd)
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

#' @describeIn divide_into_tiles Creates a full grid of overlapping polygons that covers sfobj.
#' @param points An sf object to cover with a grid. Could also be a bbox object.
#' @param cellsize is the size of a single cell
#' @param overlap is the amount of overlap of each cell
#' @return An sf multipolygon object made of squares that overlap by buffer, and cover the region bbox expanded by buffer.
#' @export
make_grid_buffer <- function(points, cellsize = 0.1, overlap = 0.01){
  # cell left hand edges are actually cellsize - buffer apart
  bbox <- sf::st_bbox(points)
  coords <- sf::st_coordinates(points)
  xmins <- seq(bbox$xmin - overlap, bbox$xmax + overlap, by = cellsize - overlap)
  ymins <- seq(bbox$ymin - overlap, bbox$ymax + overlap, by = cellsize - overlap)
  # xdiff <- Rfast::Outer(coords[, 1], xmins, oper = "-")
  # bestxminsidx <- unique(unlist(apply(xdiff, 2, function(x) {x[x > -overlap/0.99] <- NA; which.max(x)})))
  # bestxmins <- xmins[bestxminsidx]
  # ydiff <- Rfast::Outer(coords[, 2], ymins, oper = "-")
  # bestyminsidx <- unique(unlist(apply(ydiff, 2, function(x) {x[x > -overlap/0.99] <- NA; which.max(x)})))
  # bestymins <- ymins[bestyminsidx]
  
  mins <- expand.grid(xmins, ymins)
  maxs <- mins + Rfast::rep_row(matrix(cellsize, nrow = 1, ncol = 2), nrow(mins))
  
  # whittle down rectangles of use (for plausible computation speed)
  # centres <- cbind(rowMeans(cbind(mins[, 1, drop = FALSE], maxs[, 1, drop = FALSE])),
  #                  rowMeans(cbind(mins[, 2], maxs[, 2])))
  # centres <- st_as_sf(data.frame(centres), coords = c(1, 2))
  # st_crs(centres) <- st_crs(points)
  # st_distance(points, centres)
  # 
  # mins
  
  rects <- mapply(make_rectangle, xmin = mins[, 1], ymin = mins[, 2],
                                   xmax = maxs[, 1], ymax = maxs[, 2], SIMPLIFY = FALSE)
  gridman <- sf::st_sfc(rects)
  gridman <- sf::st_set_crs(gridman, sf::st_crs(points))
  return(gridman)
}

make_rectangle <- function(xmin, ymin, xmax, ymax){
  bl <- c(xmin, ymin)
  br <- c(xmax, ymin)
  tr <- c(xmax, ymax)
  tl <- c(xmin, ymax)
  mat <- rbind(bl, br, tr, tl, bl)
  out <- sf::st_polygon(list(mat))
  return(out)
}

#' @describeIn divide_into_tiles Creates a single rectangle around an sf object
#' @param sfobj An sf object
#' @param buffer A numerical distance of a buffer to include around sfobj in the output tile
#' @return an sf polygon object that is rectangular
#' @export
tilearoundobj <- function(sfobj, buffer){
  suppressMessages(wantedarea <- st_buffer(sfobj, buffer))
  tile <- st_as_sfc(st_bbox(wantedarea))
  return(tile)
}


#' @examples 
#' library(sf)
#' locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
#' locs_sf <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
#' locs_sf <- st_set_crs(locs_sf, 4326) #4326 is the epsg code for WGS84
#' tileswpts <- divide_into_tiles(locs_sf, cellsize = 1, buffer = 0.01)
#' tiles <- do.call(rbind, lapply(tileswpts, function(x) st_as_sf(x$tile)))
#' pts_l <- lapply(1:length(tileswpts), function(x) cbind(tile = x, st_as_sf(tileswpts[[x]]$pts)))
#' pts <- do.call(rbind, pts_l)
