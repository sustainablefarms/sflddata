#' @title Extract Bricks of Remote Sensing Data
#' @param spobj Spatial* object that informs extents of the raster to extract
#' @param years Years of data to extract
#' @return A raster brick with extent equal or larger than \code{extent(spobj)}, snapped to the cells of the raster data. The crs of the GPP brick is WGS84.
#' @export
brick_gpp <- function(spobj, years){
  spobj <- sp::spTransform(spobj, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  roi <- raster::extent(spobj)
  
  #prepare raster file names
  files <- build_filename_list("[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
                               "8day/GPP",
                               "OzWALD",
                               "GPP",
                               years,
                               "nc")
  
  #extract raster data given prior knowledge of format of the netCDF files
  gpp_brick <- extract_brick_files(files, "GPP", roi, dims = c(2, 1, 3),
                                          timeconvertfun = function(t) lubridate::as_date("1800-01-01") + days(t))
  return(gpp_brick)
}

#' @describeIn brick_gpp Extract brick of pg values
#' @export
brick_pg <- function(spobj, years){
  spobj <- sp::spTransform(spobj, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  roi <- raster::extent(spobj)
  
  #prepare raster file names
  files <- build_filename_list("[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
                               "daily/meteo/Pg",
                               "OzWALD.daily",
                               "Pg",
                               years,
                               "nc")
  # See http://dapds00.nci.org.au/thredds/catalog/ub8/au/OzWALD/catalog.html for more files
  
  #extract raster data given prior knowledge of format of the netCDF files
  pg_brick <- extract_brick_files(files, "Pg", roi, dims = c(2, 1, 3),
                                          timeconvertfun = function(t) lubridate::as_date("1800-01-01") + days(t))
  return(pg_brick)
}

#' @describeIn brick_gpp Extract brick of soil moisture values
#' @export
brick_ssoil <- function(spobj, years){
  spobj <- sp::spTransform(spobj, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  roi <- raster::extent(spobj)
  
  #prepare raster file names
  files <- build_filename_list("[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
                               "8day/Ssoil",
                               "OzWALD",
                               "Ssoil",
                               years,
                               "nc")
  # See http://dapds00.nci.org.au/thredds/catalog/ub8/au/OzWALD/catalog.html for more files
  
  #extract raster data given prior knowledge of format of the netCDF files
  pg_brick <- extract_brick_files(files, "Ssoil", roi, dims = c(2, 1, 3),
                                  timeconvertfun = function(t) lubridate::as_date("1800-01-01") + days(t))
  return(pg_brick)
}

#' @describeIn brick_gpp  Extract brick of 8 day fmc values
#' @export
brick_fmc <- function(spobj, years){
  spobj <- sp::spTransform(spobj, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  roi <- raster::extent(spobj)
  
  files <- build_filename_list("[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au",
                      "FMC/c6/mosaics",
                      "fmc_c6_",
                      years,
                      type_extension = ".nc",
                      namesep = "")
  
  #extract raster data given prior knowledge of format of the netCDF files
  fmc_mean_brick <- extract_brick_files(files, "fmc_mean", roi, dims = 1:3,
                      timeconvertfun = function(t) lubridate::as_date(as.POSIXlt(t, origin = lubridate::origin)))
  return(fmc_mean_brick)
}

suppressDatumDiscardWarning <- function(expr){
  out <- withCallingHandlers(expr,
                      warning = function(w){
                        if ((grepl("showSRID", w$call)[[1]]) &&
                            grepl("Discarded datum", w$message))
                          tryInvokeRestart("muffleWarning") 
                      })
  return(out)
}

