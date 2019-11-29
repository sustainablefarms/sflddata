#' @title Extact Timeseries of Values for Spatial Points
#' @description For every spatial point provided the function extracts a time series values from the given netcdf4 file.
#' @details 
#' Some netcdf4 files have dimensions ordered as 'latitude, longitude, time'.
#' The package \pkg{raster} currently has a bug and cannot extract these correctly so the function involves a work around.
#' Each extraction is checked using custom built extraction function.
#' @importFrom raster brick extract
#' @importFrom ncdf4 nc_open ncvar_get
#' @param points A Spatial Points Data Frame
#' @param filelocation A string specifying the location of the netcdf4 file
#' @param crs An object of 'CRS' class specifies the datum and projection of the netcdf4 file
#' @param varname The name of the variable in the netcdf4 file to be converted into the values of the time series.
#' @param nl  Integer. Number of layers to extract from the ncdf file. Layers 1:nl will be extracted.
#' @return A matrix. Each row corresponds to the same row in the \code{points} SPDF. Each column corresponds to a layer in the ncdf file, typically a time point..
#' @examples 
#' source("./functions/sites_2_sp_points.R")
#' sws_sites <- readRDS("./data/sws_sites.rds")
#' points <- sws_sites_2_spdf(sws_sites)
#' filelocation <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/annual/OzWALD.annual.Pg.AnnualSums.nc"
#' varname = "AnnualSums"
#' crs = CRS("+proj=longlat +datum=WGS84")
#' tseries <- extract_ts_wald(points,
#'        filelocation,
#'        varname = varname,
#'        crs = crs)
extract_ts_wald <- function(points, filelocation, varname, crs = CRS("+proj=longlat +datum=WGS84"), nl = NULL){
  nc <- nc_open(filelocation)
  stopifnot(!is.null(varname))
  
  #extract a point from a manual use of ncvar_get for later checking:
  randptidx <- sample.int(nrow(points), size = 1)
  randptcoord <- coordinates(points[randptidx, ])
  tseries_sample_manual <- extract_point_wald_manual(randptcoord[1], randptcoord[2], nc, varname = varname, nl = nl)
  
  #transform spatial points when data saved with latitude first (to get around bug in raster package code)
  dimorder <- unlist(lapply(nc$var[[varname]]$dim, function(x){x$name}))
  if ((dimorder[[1]] == "latitude") && (dimorder[[2]] == "longitude")) {
    points <- sp_2_waldncdfcoords(points, crs = crs, nc)
  }
  
  #extract points using raster package
  b <- brick(filelocation, varname = varname)
  if (is.null(nl)) {nl <- length(ncvar_get(nc, "time"))} #make default nl the full number of available time points
  tseries <- extract(b, points, nl = nl)
  row.names(tseries) <- row.names(points)
  
  #check that matches manual extraction by extracting a random point using the manual method
  if (any(abs(tseries[randptidx, ] - tseries_sample_manual) > 1E-8)){
    stop("Time series extracted using raster package does not match manual extraction test.")
    }
  
  #if no error then return result
  return(tseries)
}


#' @describeIn extract_ts_wald Transform an sp object to the spatial coordinates of a (latitude, longitude, time) ncdf file as interpreted by the package \pkg{raster}
#' @description Some ndcf files in the WALD database appear to have dimensions ordered by (latitude, longitude, time),
#' Furthermore raster reads the pixels of array data in the reverse order of the latitude values.
#' This function applies these conversions to an sp object so that the data extract through raster will match up perfectly.
#' The following takes a sp object as one would normally experience it.
#' @details The transform assumes that the way raster reads the ncdf values is such that:
#' (1) the latlong coordinates assigned to each pixel in the netcdf file are switched in order.
#' (2) the latitude in the netcdf file is reversed by the function f(x) = -x - 54 where '-54' is the sum of the end points of the range in latitudes.
#' @param spobj a SpatialPolygons, SpatialPoints, SpatialLine, or SpatialGrid etc object
#' @param crs The spatial coordinate system (in PROJ.4 format) of the ncdf file (ignoring the order of the dimensions and reversed latitudes).
#' @param nc The open ncdf file object (created using nc_open) to transform to.
#' @importFrom maptools elide
#' @importFrom sp spTransform
#' @examples 
#' countries <- rworldmap::getMap(resolution = "high") #do this for a world map polygon
#' row.names(countries) <- unlist(lapply(countries@polygons, function(x) attr(x, 'ID'))) #a polishing fix required of this data frame
#' auspoly <- countries[16, ] #Australia outline
#' sp_2_waldncdfcoords(auspoly)
sp_2_waldncdfcoords <- function(spobj, nc, crs = CRS("+proj=longlat +datum=WGS84")){
  latrange <- range(ncvar_get(nc, "latitude"))
  spobj <- spTransform(spobj, crs) #place object into the same coordinate system as the ncdf file
  spobj_rot <- elide(spobj, rotate = -90, center = c(0, 0)) #to get into the same shape as the raster reading option first rotate by 90
  spobj_latshift <- elide(spobj_rot, shift = c(sum(latrange), 0)) #then shift the latitude
  return(spobj_latshift)
}

#' @describeIn extract_ts_wald Extract values at a single point from a netCDF file in WGS84 longlat coordinates without using the \pkg{raster} package.
#' @param long Longitude of point
#' @param lat Latitude of point
#' @param nc  Open netcdf file or the location of netcdf file
#' @param nl  Integer. Number of layers to extract from the ncdf file.
#' @details Following the CF conventions for netCDF file, it is assumed that the list of longitude and latitude represents the centres of pixels.
#' The pixel that contains the point can then be found using the closest longitude and latitude, when the point is within the grid.
#' @section WARNING: this function should be checked with a person knowledgable of the WALD data and its projection.
#' @examples 
#' long <- 148.0779
#' lat <- -35.13167
#' nc <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/annual/OzWALD.annual.Pg.AnnualSums.nc"
#' extract_point_wald_manual(long, lat, nc)
extract_point_wald_manual <- function(long, lat, nc, varname, nl = NULL){
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
  
  if (is.null(nl)) {nl <- length(lyrs)} #setting defaults
  dimorder <- unlist(lapply(nc$var[[varname]]$dim, function(x){x$name}))
  
  #arranging start and count to match order of dimensions of the variable
  start <- vector("integer", length(dimorder)); names(start) <- dimorder
  start[c("latitude", "longitude", "time")] <- c(idx_lat, idx_long, 1)
  count <- vector("integer", length(dimorder)); names(count) <- dimorder
  count[c("latitude", "longitude", "time")] <- c(1, 1, nl)
  
  #reading values from ncdf file
  varvalues <- ncvar_get(nc, varid = varname, start = start, count = count)
  if(!ncin){nc_close(nc)} #if the nc file was only opened for in function then close it now
  names(varvalues) <- lyrs[1:nl]
  return(varvalues)
} 
