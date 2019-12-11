#' @title Extract 8 Day Frequency Bricks for GPP
#' @importFrom sp spTransform
#' @param spobj Spatial* object that informs extents of the raster to extract
#' @param years Years of GPP to extract
#' @value A raster brick with extent given by \code{extent(spobj)}
#' 
brick_gpp <- function(spobj, years){
  spobj <- spTransform(spobj, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  roi <- extent(spobj)
  
  #prepare raster file names
  files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
                               "8day/GPP",
                               "OzWALD",
                               "GPP",
                               years,
                               "nc")
  
  #extract raster data given prior knowledge of format of the netCDF files
  gpp_brick <- extract_brick_files(files, "GPP", roi, dims = c(2, 1, 3),
                                          timeconvertfun = function(t) as_date("1800-01-01") + days(t))
  return(gpp_brick)
}

#' @describeIn brick_gpp  Extract brick of 8 day fmc values
brick_fmc <- function(spobj, years){
  spobj <- spTransform(spobj, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  roi <- extent(spobj)
  
  files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au",
                      "FMC/c6/mosaics",
                      "fmc_c6_",
                      years,
                      type_extension = ".nc",
                      namesep = "")
  
  #extract raster data given prior knowledge of format of the netCDF files
  fmc_mean_brick <- extract_brick_files(files, "fmc_mean", roi, dims = 1:3,
                      timeconvertfun = function(t) as_date(as.POSIXlt(t, origin = lubridate::origin)))
  return(fmc_mean_brick)
}

#' @describeIn brick_gpp Extract a raster brick for Woody Cover data
brick_woodycover <- function(spobj, years){
  spobj <- spTransform(spobj, CRS("+init=epsg:3577"))
  roi <- extent(spobj)
  
  #tile codes:
  tilecodes <- get_tilecodes(spobj)
  
  #build brick for each tile
  brickfortile <- function(tilecode){
    filelist <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC",
                                    tilecode,
                                    paste0("fc_metrics_", tilecode, "_"),
                                    years,
                                    ".nc",
                                    namesep = "")
    r.l <- lapply(filelist,
                  function(x){
                    tryCatch(
                      {ras <- raster(x, varname = "WCF", dims = 2:1)
                      return(ras)
                      }
                      ,
                      warning = function(w) {
                        if (!grep("cannot process these parts of the CRS", as.character(w))){
                          warning(paste("For", x, w))
                        } else {
                          suppressWarnings(ras <- raster(x, varname = "WCF", dims = 2:1))
                        }
                      })
                  })
    bs <- brick(r.l)
    bs <- crop(bs, roi)
    names(bs) <- years
    return(bs)}
  b.l <- lapply(tilecodes, brickfortile) 
  
  # merge bricks
  b <- Reduce(merge, b.l)
  names(b) <- years
  proj4string(b) <- CRS("+init=epsg:3577")
  return(b)
}