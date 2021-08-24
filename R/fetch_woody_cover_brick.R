#' @title Extract a brick of the WALD woody cover values.
#' @description Uses the files stored at the THREDDS server at NCI.
#' @param spobj Spatial* object that informs extents of the raster to extract
#' @param years Years of data to extract
#' @return A raster brick with extent equal or larger than \code{extent(spobj)}, snapped to the cells of the raster data.
#' The projection of the returned raster is EPSG:3577, which is GDA94.
#' @export
fetch_woody_cover_brick <- function(spobj, years){
  spobj <- sp::spTransform(spobj, CRS("+init=epsg:3577"))
  roi <- raster::extent(spobj)
  
  #tile codes:
  tilecodes <- get_tilecodes(spobj)
  austiles <- unlist(read.csv(system.file("austilecodes.txt", package = "sflddata")))
  missingtiles <- setdiff(tilecodes, austiles)
  if (length(missingtiles) > 0){
    warning(paste("The following tiles are not available due to being outside of Australia mainland:",
                  paste(missingtiles, collapse = " "),
                  ". They will be treated as having zero woody canopy."))
  }
  tilecodes <- intersect(tilecodes, austiles)
  
  #build brick for each tile
  brickfortile <- function(tilecode){
    filelist <- build_filename_list("[fillmismatch]http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC",
                                    tilecode,
                                    paste0("fc_metrics_", tilecode, "_"),
                                    years,
                                    # ".nc#fillmismatch", #this code at the end get around the data type and fill value mismatch errors
                                    ".nc", 
                                    namesep = "")
    r.l <- lapply(filelist,
                  function(x){
                    ras <- withCallingHandlers(raster_wcflike(x, varname = "WCF"),
                                               warning = function(w){
                                                 if (grepl("cannot process these parts of the CRS", w$message))
                                                   tryInvokeRestart("muffleWarning") 
                                               })
                  })
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
  sp::proj4string(b) <- CRS("+init=epsg:3577")
  
  # if missing tiles, extend raster with zero values
  if (length(missingtiles) > 0){
    b <- raster::extend(b, roi, snap = "out")    
  }
  return(b)
}

#' @export
brick_woodycover <- fetch_woody_cover_brick