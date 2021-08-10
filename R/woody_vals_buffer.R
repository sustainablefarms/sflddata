#' @title Extract mean woody cover within a buffer of a point
#' @param roi is an sf object. 
#' @param pts is a set of sf points within roi
#' @param years Years of data to acquire
#' @param buffer is in METRES!
#' @return each row is a point, each column is a date
# woody_vals_buffer(tilearoundpt(locs_GDA94AA[1, ], buffer = 1000), locs_GDA94AA[1, ], 2018, buffer = 500)
#' @export
woody_vals_buffer <- function(roi, pts, years, buffer){
  suppressMessages(iscovered <- (vapply(sf::st_covered_by(pts, roi), length, FUN.VALUE = 1.1) >= 1))
  stopifnot(all(iscovered))
  roi_sp <- sf::as_Spatial(sf::st_transform(roi, 3577))
  pts_sp <- sf::as_Spatial(sf::st_transform(pts, 3577))
  if (ncol(pts_sp) == 0){pts_sp <- cbind(pts_sp, fake = NA)} #need this to use [id, ] extraction when data slot has no columns (which occurs when creating some sf objects)
  
  woody_b <- brick_woodycover(roi_sp, years) #in epsg:3577, which is GDA94
  wf <- raster::focalWeight(woody_b, d = buffer, type = "circle") # produces a warnings, but the matrix output still looks good
  woody_vals_l <- lapply(1:nrow(pts_sp), 
       function(id){
         ptbuffer <- raster::buffer(pts_sp[id, ], width = buffer * 1.2)
         smallbrick <- raster::crop(woody_b, ptbuffer)
         smallbrick[smallbrick == 157] <- 0
         smallbrick[smallbrick > 100] <- NA # I don't know what the other values above 100 mean, and if there are any
         woody_bs <- focal_bylayer(smallbrick, wf, fun = sum)
         names(woody_bs) <- names(woody_b)
         woody_vals <- raster::extract(woody_bs, pts_sp[id, ], df = TRUE)
       })
  woody_vals <- do.call(rbind, woody_vals_l)
  # woody_vals <- extract(woody_bs, pts_sp, df = TRUE)
  
  # # join to sf object
  # woody_vals_sf <- cbind(woody_vals[, -1, drop = FALSE], pts)
  return(woody_vals[, -1, drop = FALSE])
}

