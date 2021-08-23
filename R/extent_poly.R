#' @title Helper function to get the extent polygon of a raster
#' @param x A raster* object from the `raster` package
#' @examples 
#' ras <- raster::raster("./inst/demoBGGWmap.tif")
#' roi <- extent_poly(ras)
#' @export
extent_poly <- function(x){
  ras <- x
  bb <- raster::bbox(ras)
  poly <- bb_to_poly(bb)
  roi <- sf::st_as_sf(sf::st_sfc(poly), crs = raster::crs(ras))
  return(roi)
}

bb_to_poly <- function(bb){
  bb_pts <- rbind(t(bb)[1, ],
                  diag(bb),
                  t(bb)[2, ],
                  bb[c(3, 2)],
                  t(bb)[1, ])
  poly <- sf::st_polygon(x = list(bb_pts), dim = "XY")
  return(poly)
}


