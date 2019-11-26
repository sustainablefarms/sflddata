#' @title Extact Timeseries of Values for Spatial Points
#' @description For every spatial point provided the function extracts a time series values from the given netcdf4 file.
#' @importFrom raster brick extract
#' @importFrom ncdf4 nc_open ncvar_get
#' @param points A Spatial Points Data Frame
#' @param filelocation A string specifying the location of the netcdf4 file
#' @param crs An object of 'CRS' class specifies the datum and projection of the netcdf4 file
#' @param varname The name of the variable in the netcdf4 file to be converted into the values of the time series.
#' @return A matrix. Each row corresponds to the same row in the \code{points} SPDF. Each column corresponds to a layer in the ncdf file, typically a time point..
#' @examples 
#' source("./functions/sites2spatialpoints.R")
#' sws_sites <- readRDS("./data/sws_sites.rds")
#' points <- swssites2spdf(sws_sites)
#' filelocation <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/annual/OzWALD.annual.Pg.AnnualSums.nc"
#' varname = "AnnualSums"
#' crs = CRS("+proj=longlat +datum=WGS84")
#' tseries <- extract_ts_wald(points,
#'        filelocation,
#'        varname = varname,
#'        crs = crs)
extract_ts_wald <- function(points, filelocation, varname = NA, crs = CRS("+proj=longlat +datum=WGS84")){
  pointsT <- sp_2_waldncdfcoords(points, crs = crs)
  xo <- nc_open(filelocation)
  stopifnot(all(range(ncvar_get(xo, "latitude")) == c(-44, -10)))
  b <- brick(filelocation, varname = varname)
  tseries <- extract(b, pointsT)
  
  #check that matches manual extraction by extracting a random point using the manual method
  randptidx <- sample.int(nrow(points), size = 1)
  randptcoord <- coordinates(points[randptidx, ])
  tseries_sample_manual <- extract_point_wald_manual(randptcoord[1], randptcoord[2], xo, varname = varname)
  if (any(abs(tseries[randptidx, ] - tseries_sample_manual) > 1E-8)){stop("Time series extracted using raster package does not match manual extraction test.")}
  
  #if no error than return result
  return(tseries)
}


#' @describeIn extract_ts_wald Transform an sp object to the spatial coordinates of the WALD ncdf files as interpreted by the package raster
#' @description The ndcf files in the WALD database appear to have dimensions ordered by (latitude, longitude, time),
#' Furthermore raster reads the pixels of array data in the reverse order of the latitude values.
#' This function applies these conversions to an sp object so that the data extract through raster will match up perfectly.
#' The following takes a sp object as one would normally experience it.
#' @details The transform assumes that the way raster reads the ncdf values is such that:
#' (1) the latlong coordinates assigned to each pixel in the netcdf file are switched in order.
#' (2) the latitude in the netcdf file is reversed by the function f(x) = -x - 54
#' @param spobj a SpatialPolygons, SpatialPoints, SpatialLine, or SpatialGrid etc object
#' @param crs The spatial coordinate system (in PROJ.4 format) of the raster file, ignoring the flipped and order of the dimensions and reversed latitudes.
#' @importFrom maptools elide
#' @importFrom sp spTransform
#' @examples 
#' countries <- rworldmap::getMap(resolution = "high")
#' row.names(countries) <- unlist(lapply(countries@polygons, function(x) attr(x, 'ID'))) #a polishing fix required of this data frame
#' auspoly <- countries[16, ] #Australia outline
#' sp_2_waldncdfcoords(auspoly)
#' 
#' source("./functions/sites2spatialpoints.R")
#' sws_sites <- readRDS("./data/sws_sites.rds")
#' points <- swssites2spdf(sws_sites)
#' sp_2_waldncdfcoords(points)
sp_2_waldncdfcoords <- function(spobj, crs = CRS("+proj=longlat +datum=WGS84")){
  spobj <- spTransform(spobj, crs)
  spobj_rot <- elide(spobj, rotate = -90, center = c(0, 0))
  spobj_latshift <- elide(spobj_rot, shift = c(-44 - 10, 0))
  return(spobj_latshift)
}

#' @describeIn extract_ts_wald Extract a Single Point Value from a WALD netCDF file without using the \pkg{raster} package.
#' @param long Longitude of point
#' @param lat Latitude of point
#' @param nc  Open netcdf file or the location of netcdf file
#' @description The list of longitude and latitude appear to represent the centres of pixels.
#' The pixel that contains the point can be found using the closest longitude and latitude, when the point is within the grid.
#' @section WARNING: this function should be checked with a person knowledgable of the WALD data and its projection.
#' @examples 
#' long <- 148.0779
#' lat <- -35.13167
#' nc <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/annual/OzWALD.annual.Pg.AnnualSums.nc"
#' extract_point_wald_manual(long, lat, nc)
extract_point_wald_manual <- function(long, lat, nc, varname = NA){
  ncin <- TRUE  #to record if the nc file is already open
  if ("ncdf4" != class(nc)){ncin <- FALSE; nc <- nc_open(nc)}
  longs <- ncvar_get(nc, "longitude")
  lats <- ncvar_get(nc, "latitude")
  lyrs <- ncvar_get(nc, "time")
  
  longres <- (max(longs) - min(longs))/length(longs)
  latres <- (max(lats) - min(lats))/length(lats)
  longdiffs <- abs(long - longs)
  latdiffs <- abs(lat - lats)
  
  #check that point is within domain of netCDF file
  stopifnot(min(longdiffs) <= longres * (0.5 + 1E-6))
  stopifnot(min(latdiffs) <= latres * (0.5 + 1E-6))
  
  # closest pixel index
  idx_long <- which.min(abs(long - longs))
  idx_lat <- which.min(abs(lat - lats))
  
  varvalues <- ncvar_get(nc, varid = varname, start = c(idx_lat, idx_long, 1), count = c(1, 1, length(lyrs)))
  if(!ncin){nc_close(nc)} #if the nc file was only opened for in function then close it now
  names(varvalues) <- lyrs
  return(varvalues)
} 
