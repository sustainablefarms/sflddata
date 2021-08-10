#' @title Get raster values from the WCF style of netCDF files
#' @description Raster, GDAL and Terra do not read the WCF and similar files properly.
#' I suspect this is because Matlab produces netCDF files with dimension in an order inconsistent with GDAL and raster: x-dimension first and then y-dimension.
#' There seems to be unreliability with using raster::t() and raster::flip().
#' Instead this function reads the values as a netcdf file.
#' @param file NetCDF file name 
#' @param varname Variable name to extract from the netCDF file (can only extract one variable at a time).
#' @examples 
#' file <- "[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC/17_-39/fc_metrics_17_-39_2000.nc"
#' varname <- "WCF"
#' r <- raster_wcflike(file, varname = "WCF")
#' 
#' syd_latlong <- SpatialPoints(matrix(c(151.209900,  -33.865143), nrow = 1 ), proj4string = CRS("+proj=longlat +datum=WGS84"))
#' syd_epsg3577 <- spTransform(syd_latlong, CRS("+init=epsg:3577"))
#' plot(add = TRUE, syd_epsg3577)
#' @export
raster_wcflike <- function(file, varname){
  nc <- ncdf4::nc_open(file)
  
  # get projection
  crs_wkt <- ncdf4::ncatt_get(nc, varid = "crs")$crs_wkt
  p4s <- rgdal::showP4(crs_wkt)
  
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
         crs = p4s)
  } else {
    stop("Only implemented for xstep > 0 and ystep < 0")
  }
  
  return(ras)
}

