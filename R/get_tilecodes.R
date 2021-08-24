#' @title Intersecting Albers Australian Tile Codes
#' @description Given a spatial object returns the tile codes for Albers Tiles used by Geoscience Australia and others
#' @param spobj an sp object
#' @return a named vector of tile codes
#' @export
get_tilecodes <- function(spobj){
  spobj <- sp::spTransform(spobj, CRS("+init=epsg:3577")) #transform to the correct projection
  roi <- raster::extent(spobj)
  
  tilestep <- 100000
  lxmin <- floor(roi@xmin / tilestep) * tilestep #lowest xmin
  xmins <- seq(lxmin, -1 + ceiling(roi@xmax / tilestep) * tilestep,
               by = tilestep)
  lymin <- floor(roi@ymin / tilestep) * tilestep #lowest ymin
  ymins <- seq(lymin, -1 + ceiling(roi@ymax / tilestep) * tilestep,
               by = tilestep)
  
  xmin_v_ymin <- expand.grid(xmin = xmins, ymin = ymins)
  tilecodes <- apply(xmin_v_ymin / tilestep, 1, function(x) paste(x, collapse = "_"))
  names(tilecodes) <- tilecodes
  return(tilecodes)
}

#' @examples
#' tilecodes <- unlist(read.csv(system.file("austilecodes.txt", package = "sflddata")))
#' names(tilecodes) <- NULL
#' tiles <- tiles_as_sf(tilecodes)
#' ausstates <- readRDS("./private/data/basemaps/ausstates.rds") 
#' sf::st_crs(ausstates) <- sf::st_crs(ausstates)
#' ausstates <- sf::st_transform(ausstates, sf::st_crs(tiles))
#' # check that tiles cover all land mass
#' library(ggplot2)
#' ggplot() +
#'   geom_sf(aes(), fill = "red", data = ausstates) +
#'   geom_sf(aes(), data = tiles)

#' @describeIn get_tilecodes Make sf polygons showing the tiles
#' @param tilecodes A list of tile codes.
#' @export
tiles_as_sf <- function(tilecodes){
  tilestep <- 100000
  splitted <- strsplit(tilecodes, split = "_")
  xmins <- as.numeric(vapply(splitted, function(x) x[[1]], FUN.VALUE = "6")) * tilestep
  ymins <- as.numeric(vapply(splitted, function(x) x[[2]], FUN.VALUE = "6")) * tilestep
  xmaxs <- xmins + tilestep
  ymaxs <- ymins + tilestep
  tile_extents <- data.frame(
    tile = tilecodes, xmin = xmins, ymin = ymins, xmax = xmaxs, ymax = ymaxs)
  polys <- apply(tile_extents[, -1], MARGIN = 1, function(x) {
    bb <- minmaxs_to_bb(x)
    poly <- bb_to_poly(bb)
    return(poly)})
  polys_sfc <- sf::st_sfc(polys, crs = 3577)
  tiles <- sf::st_sf(tile_extents, polys_sfc)
  return(tiles)
}

minmaxs_to_bb <- function(x){
  bbform <- matrix(x, nrow = 2, ncol = 2, byrow = FALSE)
  colnames(bbform) <- c("min", "max")
  rownames(bbform) <- c("s1", "s2")
  return(bbform)
}
