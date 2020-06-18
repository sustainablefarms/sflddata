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

#' @describeIn brick_gpp Extract a raster brick for Woody Cover data
brick_woodycover <- function(spobj, years){
  if (packageVersion("raster") != "3.0-7") {
    stop(paste("Function uses the 'dims' argument of raster(). This argument requires an unofficial version of the raster package to work properly.",
    "To install this version of raster run:\n remotes::install_github('https://github.com/kasselhingee/raster', ref = 'ce63b218')"))
  }
  if (2019 %in% years) {warning("The data for year 2019 is saved differently. The results for this year will be erroneous!")}
  spobj <- sp::spTransform(spobj, CRS("+init=epsg:3577"))
  roi <- raster::extent(spobj)
  
  #tile codes:
  tilecodes <- get_tilecodes(spobj)
  
  #build brick for each tile
  brickfortile <- function(tilecode){
    filelist <- build_filename_list("[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC",
                                    tilecode,
                                    paste0("fc_metrics_", tilecode, "_"),
                                    years,
                                    # ".nc#fillmismatch", #this code at the end get around the data type and fill value mismatch errors
                                    ".nc", 
                                    namesep = "")
    r.l <- lapply(filelist,
                  function(x){
                    tryCatch(
                      {ras <- raster::raster(x, varname = "WCF", dims = 2:1)
                      return(ras)
                      }
                      ,
                      warning = function(w) {
                        if (!grep("cannot process these parts of the CRS", as.character(w))){
                          warning(paste("For", x, w))
                        } else {
                          suppressWarnings(ras <- raster::raster(x, varname = "WCF", dims = 2:1))
                        }
                      })
                  })
    r.l_crop <- lapply(r.l, raster::crop, y = roi)
    # warning: I think the following bricks get saved to rasterOptions()$tmpdir when RAM runs out
    bs <- raster::brick(r.l_crop)
    names(bs) <- years
    return(bs)}
  b.l <- lapply(tilecodes, brickfortile) 
  
  # merge bricks
  b <- Reduce(raster::merge, b.l)
  names(b) <- years
  sp::proj4string(b) <- CRS("+init=epsg:3577")
  return(b)
}