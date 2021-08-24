#' @title Extract mean woody cover within buffers for a set of points
#' @param roi is an sf object. 
#' @param pts is a set of sf points within roi
#' @param years Years of data to acquire
#' @param buffers Multiple buffers given in METRES.
#' @return A list corresponding to each buffer distance. In each element each row is a point, each column is a date
#' @examples 
#' locations <- read.csv("./private/data/clean/site_locations_cleaned.csv")
#' pts <- sf::st_as_sf(locations, coords = c("MeanLON", "MeanLAT"), crs = 4326)[1:2, ]
#' years <- 2000:2001
#' buffers <- c(100, 500)
#' global_muffle_discarddatumwarn()
#' fetch_woody_cover_meanbuffer(pts, years, buffers)
#' @export
fetch_woody_cover_meanbuffer <- function(pts, years, buffers){
  pts_3577 <- sf::st_transform(pts, 3577)
  bufferroi <- sf::as_Spatial(sf::st_buffer(pts_3577, max(buffers)))
  
  pts_sp <- sf::as_Spatial(pts_3577)

  woody_b <- fetch_woody_cover_brick(bufferroi, years) #in epsg:3577, which is GDA94
  
  wcfproportions_l <- lapply(buffers, function(buff){
    out <- raster::extract(woody_b, pts_sp, buffer = buff,
                    fun = mean,
                    small = TRUE, na.rm = TRUE, df = TRUE)
    cbind(buffer = buff, out[, -1, drop = TRUE]) #-1 removes the ID column
  })
  names(wcfproportions_l) <- buffers
  return(wcfproportions_l)
}


#' @export
woody_vals_buffer <- function(roi, pts, years, buffer){
  suppressMessages(iscovered <- (vapply(sf::st_covered_by(pts, roi), length, FUN.VALUE = 1.1) >= 1))
  stopifnot(all(iscovered))
  out <- fetch_woody_cover_meanbuffer(pts, years, buffer)
  return(out[[1]][, -1]) # to make compatible with original outputs
}