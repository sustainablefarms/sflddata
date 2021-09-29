#' @title Extract a brick of the WALD woody cover values.
#' @description Uses the files stored at the THREDDS server at NCI.
#' @param spobj Spatial* or sf object that informs extents of the raster to extract
#' @param years Years of data to extract
#' @param rootdir Where the tiles are located. Typically [fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC,
#'  however I have had need to download the tiles wholescale using [download_WCF_tiles()] and access them locally.
#' @return A raster brick with extent equal or larger than \code{extent(spobj)}, snapped to the cells of the raster data.
#' The projection of the returned raster is EPSG:3577, which is GDA94.
#' @details Combines the tile file naming convention with [raster_wcflike()] and [fetch_brick_Albers()]
#' @examples 
#' polypts <- matrix(data = c(1590555, -3670100,
#'                            1610200, -3670100,
#'                            1610200, -3664500,
#'                            1590555, -3664500,
#'                            1590555, -3670100), byrow = TRUE, ncol = 2)
#' demoshape <- sf::st_sfc(sf::st_polygon(x = list(polypts), dim = "XY"), crs = 3577)
#' b <- fetch_woody_cover_brick(demoshape, 1990)
#' @export
fetch_woody_cover_brick <- function(spobj, years, rootdir = "[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC"){
  
  b <- fetch_brick_Albers(spobj, years,
                     get_tile_filenames = function(tilecode, years){
                       filelist <- get_tile_filenames_WCF(tilecode, years, rootdir = rootdir)
                       return(filelist)}, 
                     tilereader = tilereader_WCF)
  return(b)
}

get_tile_filenames_WCF <- function(tilecode, years, rootdir = NULL){
  filelist <- build_filename_list(rootdir,
                                  tilecode,
                                  paste0("fc_metrics_", tilecode, "_"),
                                  years,
                                  # ".nc#fillmismatch", #this code at the end get around the data type and fill value mismatch errors
                                  ".nc", 
                                  namesep = "")
  return(filelist)
}

tilereader_WCF <- function(filename){
  ras <- withCallingHandlers(raster_wcflike(filename, varname = "WCF"),
                             warning = function(w){
                               if (grepl("cannot process these parts of the CRS", w$message))
                                 tryInvokeRestart("muffleWarning") 
                             })
  return(ras)
}

#' @export
brick_woodycover <- fetch_woody_cover_brick