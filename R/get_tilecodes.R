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