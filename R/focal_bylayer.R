#' @title Apply raster's focal function by layer
#' @importFrom raster focal brick stack compareRaster
#' @description Apply's raster's \code{focal} function to layers in a brick or stack. See help for \code{focal} for more information on paramater behaviour.
#' @param x A RasterBrick or RasterStack
#' @param w Focal weights as required by \code{focal}
#' @param fun Function to apply to value in focal region
#' @param na.rm If TRUE NA values removed from computations.
#' @param pad Passed to [raster::focal()]. If TRUE, additional 'virtual' rows and columns are padded to x such that there are no edge effects. This can be useful when a function needs to have access to the central cell of the filter.
#' @param padValue Passed to [raster::focal()]. The value of the cells of the padded rows and columns.
#' @param NAonly Passed to [raster::focal()]. If TRUE, only cell values that are NA are replaced with the computed focal values.
#' @param cl a cluster for parallel computation created par \pkg{parallel}. If NULL then no parallel computations will occur.
#' @param verbose If TRUE the start of each layer will be announced via a `message()`
#' @return A RasterBrick with each layer the output [raster::focal()] applied to a layer of the input brick
#' @export
focal_bylayer <- function(x, w, fun, na.rm=FALSE, pad=FALSE, padValue=NA, NAonly=FALSE, cl = NULL, verbose = FALSE, ...){
  fnctoapply <- function(n) {
	  if (verbose){
		  message(sprintf("Started layer %s", names(x)[[n]]))
	  }
	  focal(x[[n]], w = w, fun = fun,
                                  filename = '',
                                  na.rm = na.rm, pad = pad,
                                  padValue = padValue, NAonly = NAonly)}
  if (is.null(cl)) {
    r.l <- lapply(1:raster::nlayers(x), 
                  fnctoapply)
    }  else {
      clusterExport(cl = cl,
                    varlist = c("fnctoapply", "focal", "fun", "x", "w", "na.rm", "pad", "padValue", "NAonly"),
                    envir = environment()
                    )
      r.l <- parLapply(cl = cl, 1:raster::nlayers(x), 
                    fun = fnctoapply)
  }
  if ("RasterBrick" %in% class(x)) {
    xout <- brick(r.l)
  } else if ("RasterStack" %in% class(x)) {
    xout <- stack(r.l)
  } else if ("RasterLayer" %in% class(x)) {
    xout <- r.l[[1]]
  }
  names(xout) <- names(x)
  compareRaster(x, xout,
                extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, orig = TRUE,
                rotation = TRUE, values = FALSE, stopiffalse = TRUE, showwarning = FALSE)
  return(xout)
}
