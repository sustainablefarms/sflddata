#' @title Get raster values from the WCF style of netCDF files
#' @description Raster, GDAL and Terra do not read the WCF and similar files properly.
#' I suspect this is because Matlab produces netCDF files with dimension in an order inconsistent with GDAL and raster: x-dimension first and then y-dimension.
#' There seems to be unreliability with using raster::t() and raster::flip().
#' Instead this function reads the values as a netcdf file.
#' @param file NetCDF file name 
#' @param varname Variable name to extract from the netCDF file (can only extract one variable at a time).
#' @return A raster file in GDA94 / Australian Albers (EPSG:3577).
#' @details 
#' At this point it seems like proj.4 string are still in use by `raster`, even though they seem be obsolete, even according to `raster` and `sp`.
#' That means some of the WKT info in the WCF file is discarded and the super fine accuracy will be lost.
#' See https://cran.r-project.org/web/packages/rgdal/vignettes/PROJ6_GDAL3.html#Status_22_April_2020
#' @examples 
#' file <- "[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC/17_-39/fc_metrics_17_-39_2000.nc"
#' varname <- "WCF"
#' r <- raster_wcflike(file, varname = "WCF")
#' 
#' syd_latlong <- sf::st_sfc(sf::st_point(x = c(151.209900,  -33.865143), dim = "XY"),
#'       crs = 4326)
#' syd_ras <- sf::st_transform(syd_latlong, raster::crs(r))
#' 
#' raster::plot(r)
#' plot(add = TRUE, syd_ras)
#' @export
raster_wcflike <- function(file, varname){
  nc <- ncdf4::nc_open(file)
  
  # get projection
  crs_wkt <- ncdf4::ncatt_get(nc, varid = "crs")$crs_wkt
  emptysf_withcrs <- sf::st_sfc(crs = crs_wkt) 
  # using an empty sf object above avoids a Discarded datum warning when building the raster::raster object below
  # I'm not sure why this is. Passing the crs_wkt direct seems to end up running sp::CRS(SRS_string = crs_wkt) which 
  # generates the same warning. Similarly rgdal::checkCRSArgs_ng(SRS_string = crs_wkt) also gives a warning.
  # I am confused what is happening here. Is sf being lax about warning?
  
  # geotranform specifies the x and y coordinate values, but I couldn't see any descriptions of the numbers
  geotransform <- ncdf4::ncatt_get(nc, varid = "crs")$GeoTransform
  xstart <- geotransform[[1]]
  xstep <- geotransform[[2]]
  stopifnot(geotransform[[3]] == 0)
  ystart <- geotransform[[4]]
  stopifnot(geotransform[[5]] == 0)
  ystep <- geotransform[[6]]
  

  
  # get a matrix of the values
  varvals <- ncdf4::ncvar_get(nc, varid = varname)
  # graphics::image(varvals, useRaster = TRUE) #values are shown rotate counter-clockwise by 90 degrees, from the way a matrix would be printed.
  # I think that means the top row of varvals is plotted to the left-most column in the image.
  
  # I *guess* that varvals has increasing row index corresponding to increasing 2nd coordinate (y) and 
  # increasing column index corresponds to increasing 1st coordinate (x).
  # That the rows increment with the *second* coordinate must be what makes these files strange.
  
  # if xstep > 0 and ystep < 0 then printing the varvals matrix using regular method displays values as they are arranged geographically
  if ((xstep > 0) & (ystep < 0)){
    ras <- raster::raster(varvals,
         xmn = xstart, xmx = xstart + xstep * ncol(varvals),
         ymn = ystart + ystep * nrow(varvals), ymx = ystart,
         crs = emptysf_withcrs)
  } else {
    stop("Only implemented for xstep > 0 and ystep < 0")
  }
  
  return(ras)
}

