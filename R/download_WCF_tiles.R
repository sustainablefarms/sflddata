#' @title Download Woody Cover Tiles
#' @param spobj Spatial* or sf object that informs extents of the raster to extract
#' @param years Years of data to download
#' @param rootdir Root directory to copy tile structure to
#' @param ... Passed to [download.file()]
#' @description Downloads the woody cover tiles and saves them in the same directory structure as on the NCI THREDDS
#' @return A list giving success or otherwise of file downloads. Code 0 for success - see [download.file()] return value.
#' @examples 
#' polypts <- matrix(data = c(1590555, -3670100,
#'                            1610200, -3670100,
#'                            1610200, -3664500,
#'                            1590555, -3664500,
#'                            1590555, -3670100), byrow = TRUE, ncol = 2)
#' spobj <- sf::st_sfc(sf::st_polygon(x = list(polypts), dim = "XY"), crs = 3577)
#' download_WCF_tiles(spobj, years = 2000:2001)
#' @export
download_WCF_tiles <- function(spobj, years, rootdir = ".", ...){
  if (!any(grepl("Spatial.*", class(spobj)))){
    spobj <- sf::as_Spatial(spobj)
  }
  spobj <- sp::spTransform(spobj, sp::CRS("+init=epsg:3577"))
  
  tilecodes <- get_tilecodes(spobj)
  austiles <- unlist(read.csv(system.file("austilecodes.txt", package = "sflddata")))
  missingtiles <- setdiff(tilecodes, austiles)
  if (length(missingtiles) > 0){
    warning(paste("The following tiles are not available due to being outside of Australia mainland:",
                  paste(missingtiles, collapse = " "),
                  "."))
  }
  tilecodes <- intersect(tilecodes, austiles)
  
  names(tilecodes) <- tilecodes
  files_online <- unlist(lapply(tilecodes, online_tile_filenames_WCF, years = years))
  files_local <- gsub("^http://dapds00.nci.org.au/thredds/fileServer/ub8/au/LandCover/DEA_ALC", 
                      rootdir, files_online)
  
  # make directories
  tiledirs <- file.path(rootdir, tilecodes)
  out <- lapply(tiledirs, dir.create, recursive = TRUE)
  
  #download the files
  if (sum(file.exists(files_local)) > 0){
    warning(paste("Files already exist and will not be created:",
                  paste(files_local[file.exists(files_local)], collapse = ", ")))
    files_online <- files_online[!file.exists(files_local)]
    files_local <- files_local[!file.exists(files_local)]
  }

  out2 <- pbapply::pbmapply(function(url, destfile){
    out <- download.file(url, destfile, ...)
    return(out)},
                 url = files_online,
                 destfile = files_local)
  names(out2) <- files_online
  return(out2)
}

online_tile_filenames_WCF <- function(tilecode, years){
  #https://dapds00.nci.org.au/thredds/fileServer/ub8/au/LandCover/DEA_ALC/-10_-23/fc_metrics_-10_-23_1990.nc 
  filenames <- get_tile_filenames_WCF(tilecode, years)
  filenames <- gsub("^\\[fillmismatch\\]", "", filenames)
  filenames <- gsub("dodsC", "fileServer", filenames)
  names(filenames) <- years
  return(filenames)
}

local_tile_filenames_WCF <- function(tilecode, years, root = "."){
  filenames <- online_tile_filenames_WCF(tilecode, years)
  filenames <- gsub("^http://dapds00.nci.org.au/thredds/fileServer/ub8/au/LandCover/DEA_ALC", root, filenames)
  return(filenames)
}
