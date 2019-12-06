#' @title Extract RasterBrick from List of Filenames
#' @importFrom raster brick crop
#' @importFrom lubridate as_date origin
#' @param files A character list of filenames
#' @param varname Character. Variable name to extract from each file.
#' @param roi The region to extract. A \pkg{raster} 'extent' object.
#' @param dims The order of dimensions that the variable is saved in. dims = c(1, 2, 3) is [longitude, latitude, time]. dims = c(2, 1, 3) is [latitude, longitude, time].
#' @param timeconvertfun A function that converts from time values, as given in the netCDF file, to time objects
#' @examples 
#' roi <- extent(148.0779, 148.2011, -35.06581, -35.13167)
#' files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
#'       "8day/GPP",
#'       "OzWALD",
#'       "GPP",
#'       2000:2018,
#'       "nc")
#' b <- extract_brick_files(files, "GPP", roi, dims = c(2, 1, 3))


extract_brick_files <- function(files, varname, roi, dims = 1:3,
                                timeconvertfun = function(t) as_date(as.POSIXlt(t, origin = lubridate::origin))){
  #extract raster in region of interest and combine into a larger raster
  roiras.l <- lapply(files, function(x) {
    tryCatch(
      {b <- brick(x, varname = varname, dims = dims)
      roiras <- crop(b, roi, snap = "out")
      times <- ncvar_get(nc_open(x), "time")
      times <- do.call(timeconvertfun, list(t = times))
      names(roiras) <- times
      },
      error = function(cond){
        message(paste("Error in reading", x))
        message(cond)
        return(NULL)
      })
    return(roiras)})
  
  return(brick(roiras.l))
}
