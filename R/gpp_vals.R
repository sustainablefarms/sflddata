#' @title Extract GPP values at points
#' @param roi is an sf object. 
#' @param pts is a set of sf points within roi
#' @param years Years of data to acquire
#' @return each row is a point, each column is a date
# gpp_vals(tilearoundpt(locs_GDA94AA[1, ], buffer = 1000), locs_GDA94AA[1:1, ], 2019)
#' @export
gpp_vals <- function(roi, pts, years){
  suppressMessages(iscovered <- (vapply(st_covered_by(pts, roi), length, FUN.VALUE = 1.1) >= 1))
  stopifnot(all(iscovered))
  roi_sp <- sf::as_Spatial(st_transform(roi, 4326)) #gpp is in WGS84
  pts_sp <- sf::as_Spatial(st_transform(pts, 4326))
  gpp_b <- brick_gpp(roi_sp, years) #gpp is in WGS84
  gpp_vals <- extract(gpp_b, pts_sp, df = TRUE)
  
  # # join to sf object
  # gpp_vals_sf <- st_set_geometry(gpp_vals[, -1], st_geometry(pts))
  return(gpp_vals[, -1, drop = FALSE])
}