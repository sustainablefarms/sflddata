#' @title Extract RasterBrick from List of Filenames
#' @importFrom raster brick crop
#' @importFrom lubridate as_date origin
#' @param files A character list of filenames
#' @param varname Character. Variable name to extract from each file.
#' @param roi The region to extract. A \pkg{raster} 'extent' object.
#' @param dims The order of dimensions that the variable is saved in. dims = c(1, 2, 3) is [longitude, latitude, time]. dims = c(2, 1, 3) is [latitude, longitude, time].
#' @param timeconvertfun A function that converts from time values, as given in the netCDF file, to time objects
#' @examples 
#' roi <- extent(148.0779, 148.2011, -35.26581, -35.13167)
#' files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD",
#'       "8day/GPP",
#'       "OzWALD",
#'       "GPP",
#'       2000:2018,
#'       "nc")
#' b <- extract_brick_files(files, "GPP", roi, dims = c(2, 1, 3))


extract_brick_files <- function(files, varname, roi, dims = 1:3,
                                timeconvertfun = function(t) as_date(as.POSIXlt(t, origin = lubridate::origin))){
  if (packageVersion("raster") != "3.0-7") {
    stop(paste("Function uses the 'dims' argument of brick(). This argument requires an unofficial version of the raster package to work properly.",
    "To install this version of raster run:\n remotes::install_github('https://github.com/kasselhingee/raster', ref = 'ce63b218')"))
  }

  #extract raster in region of interest and combine into a larger raster
  roiras.l <- lapply(files, readcropbrick,
                     varname = varname, dims = dims, roi = roi, timeconvertfun = timeconvertfun)
  extents <- lapply(roiras.l, raster::extent)
  uextents <- unique(extents)
  if (length(uextents) > 1) {
    bricks <- lapply(uextents, function(x) {
      brick(roiras.l[vapply(extents, function(y) identical(y, x), FUN.VALUE = FALSE)])
      })
    return(bricks)
  }
  return(brick(roiras.l))
}

readcropbrick <- function(x, varname, dims, roi, timeconvertfun) {
  tryCatch({
      b <- brick(x, varname = varname, dims = dims, stopIfNotEqualSpaced = TRUE) #stopIfNotEqualSpaced means non-equally spaced netCDF values cause a warning rather than an errror
      roiras <- crop(b, roi, snap = "out")
      times <- ncdf4::ncvar_get(ncdf4::nc_open(x), "time")
      times <- do.call(timeconvertfun, list(t = times))
      names(roiras) <- times
      return(roiras)},
      error = function(e) {
        stop("Error reading ", x, ":\n", e)
      }
      )
}
