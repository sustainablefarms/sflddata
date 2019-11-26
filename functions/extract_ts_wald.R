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
  vals <- extract(b, pointsT)
  return(vals)
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