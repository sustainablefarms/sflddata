#' @title Extract a brick of Australian Albers Tiles.
#' @description Uses the files stored at the given location, and assumes the tiles are saved as EPSG:3577, which is GDA94.
#' @param spobj Spatial* or sf object that informs extents of the raster to extract
#' @param years Years of data to extract
#' @param get_tile_filenames A function with arguments (tilecode, years, ...). 
#' For a given tilecode and years it must return a list of filenames. Each filename is for the tile for each year provided.
#' @param tilereader A function that accepts a single argument, 'filename', and returns a raster object for that file.
#' @return A raster brick with extent equal or larger than \code{extent(spobj)}, snapped to the cells of the raster data.
#' The projection of the returned raster is EPSG:3577, which is GDA94.
#'  Extent of the returned value is a rectangle, and pixel values outside `spobj` are included.
#' @examples
#' 
#' @export
fetch_brick_Albers <- function(spobj, years, get_tile_filenames = get_bggwtile_filenames, tilereader = bggwtilereader){
  if (!any(grepl("Spatial.*", class(spobj)))){
    spobj <- sf::as_Spatial(spobj)
  }
  spobj <- sp::spTransform(spobj, sp::CRS("+init=epsg:3577"))
  roi <- raster::extent(spobj)
  
  #tile codes:
  tilecodes <- get_tilecodes(spobj)
  austiles <- unlist(read.csv(system.file("austilecodes.txt", package = "sflddata")))
  missingtiles <- setdiff(tilecodes, austiles)
  if (length(missingtiles) > 0){
    warning(paste("The following tiles are not available due to being outside of Australia mainland:",
                  paste(missingtiles, collapse = " "),
                  ". The returned raster will have NA values for locations in these tiles."))
  }
  tilecodes <- intersect(tilecodes, austiles)
  
  #build brick for each tile
  brickfortile <- function(tilecode){
    filelist <- get_tile_filenames(tilecode, years)
    r.l <- lapply(filelist, tilereader)

    names(r.l) <- years
    r.l_crop <- lapply(r.l, raster::crop, y = roi, snap = "out")

    # warning: I think the following bricks get saved to rasterOptions()$tmpdir when RAM runs out
    bs <- raster::brick(r.l_crop)
    names(bs) <- years
    return(bs)}
  b.l <- lapply(tilecodes, brickfortile) 
  
  # merge bricks
  b <- Reduce(raster::merge, b.l)
  names(b) <- years
  sp::proj4string(b) <- sp::CRS("+init=epsg:3577")
  
  # if missing tiles, extend raster with zero values
  if (length(missingtiles) > 0){
    b <- raster::extend(b, roi, value = NA, snap = "out")    
  }
  return(b)
}

#' @export
fetch_brick <- fetch_brick_Albers