#' @title Functions for using and then deleting new temporary directories
#' @description The raster package does large computations by saving data temporarily to the computer harddisk.
#' However, the package does not know when to delete these files so they are left there.
#' For large data computations the hard drive can quickly fill with intermediate data files.
#' Any data that you want to save in the temporary directory must moved elsewhere before deleting the
#' temporary directory.
#' @param origdir Original temporary raster directory
#' @param newdir  New directory name within the current raster temporary direcotory.
#' If NULL it will be generated on creation.
#' 
#' @examples 
#' dirs <- rastertmpdir_movetonew()
#' rastertmpdir_movetoorig(dirs$orig)
#' # shift data to save
#' rastertmpdir_removenew(dirs$new)
#' @export
rastertmpdir_movetonew <- function(newdir = NULL){
  origrastertmpdir <- raster::tmpDir() # create new temporary directories - for deleting later
  if (is.null(newdir)){
    newdir <- tempfile(pattern = "tmpdata", tmpdir = origrastertmpdir)
  } else {
    newdir <- paste0(origrastertmpdir, newdir)
  }
  dir.create(newdir)
  raster::rasterOptions(tmpdir = newdir)
  
  return(list(orig = origrastertmpdir,
              new = newdir))
}

#' @describeIn rastertmpdir_movetonew Switches raster temporary directory back to original directory.
#' @export
rastertmpdir_movetoorig <- function(origdir){
  raster::rasterOptions(tmpdir = origdir)
}

#' @describeIn rastertmpdir_movetonew Deletes the temporary directory.
#' @export
rastertmpdir_removenew <- function(newdir){
  unlink(newdir, recursive = TRUE)
}

