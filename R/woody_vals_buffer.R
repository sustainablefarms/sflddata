#' @title Extract mean woody cover within a buffer of a point
#' @param roi is an sf object. 
#' @param pts is a set of sf points within roi
#' @param years Years of data to acquire
#' @param buffer is in METRES!
#' @return each row is a point, each column is a date
# woody_vals_buffer(tilearoundpt(locs_GDA94AA[1, ], buffer = 1000), locs_GDA94AA[1, ], 2018, buffer = 500)
#' @export
woody_vals_buffer <- function(roi, pts, years, buffer){
  suppressMessages(iscovered <- (vapply(st_covered_by(pts, roi), length, FUN.VALUE = 1.1) >= 1))
  stopifnot(all(iscovered))
  roi_sp <- as_Spatial(st_transform(roi, 3577)) #woody_b is in epsg:3577, which is GDA94
  pts_sp <- as_Spatial(st_transform(pts, 3577))
  
  woody_b <- brick_woodycover(roi_sp, years) #in epsg:3577, which is GDA94
  wf <- raster::focalWeight(woody_b, d = buffer, type = "circle") # produces a warnings, but the matrix output still looks good
  woody_vals_l <- lapply(1:nrow(pts_sp), 
       function(id){
         ptbuffer <- buffer(pts_sp, width = buffer * 1.2)
         smallbrick <- crop(woody_b, ptbuffer)
         woody_bs <- focal_bylayer(smallbrick, wf, fun = sum)
         names(woody_bs) <- names(woody_b)
         woody_vals <- extract(woody_bs, pts_sp, df = TRUE)
       })
  woody_vals <- do.call(rbind, woody_vals_l)
  # woody_vals <- extract(woody_bs, pts_sp, df = TRUE)
  
  # # join to sf object
  # woody_vals_sf <- cbind(woody_vals[, -1, drop = FALSE], pts)
  return(woody_vals[, -1, drop = FALSE])
}

