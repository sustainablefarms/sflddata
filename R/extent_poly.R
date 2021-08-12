#' @title Helper function to get the extent polygon of a raster
#' @param ras
#' @examples 
#' ras <- raster::raster("./inst/demoBGGWmap.tif")
#' roi <- extent_poly(ras)

extent_poly <- function(x){
  ras <- x
  bb <- raster::bbox(ras)
  bb_pts <- rbind(t(bb)[1, ],
   diag(bb),
   t(bb)[2, ],
   bb[c(3, 2)],
   t(bb)[1, ])
  poly <- sf::st_polygon(x = list(bb_pts), dim = "XY")
  roi <- sf::st_as_sf(sf::st_sfc(poly), crs = raster::crs(ras))
  return(roi)
}
