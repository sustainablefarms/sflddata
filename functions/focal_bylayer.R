#' @title Apply raster's focal function by layer
#' @importFrom raster focal brick stack compareRaster
#' @description Apply's raster's \code{focal} function to layers in a brick or stack. See help for \code{focal} for more information on paramater behaviour.
#' @param x A RasterBrick or RasterStack
#' @param w Focal weights as required by \code{focal}
#' @param fun Function to apply to value in focal region
#' @param na.rm If TRUE NA values removed from computations.
#' @param pad
#' @param padValue
#' @param NAonly
#' 

focal_bylayer <- function(x, w, fun, na.rm=FALSE, pad=FALSE, padValue=NA, NAonly=FALSE, ...){
  r.l <- lapply(1:nlayers(x), 
                function(n) focal(x[[n]], w = w, fun = fun,
                                  filename = '',
                                  na.rm = na.rm, pad = pad,
                                  padValue = padValue, NAonly = NAonly))
  if ("RasterBrick" %in% class(x)) {
    xout <- brick(r.l)
  } else if ("RasterStack" %in% class(x)) {
    xout <- stack(r.l)
  }
  names(xout) <- names(x)
  compareRaster(x, xout,
                extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, orig = TRUE,
                rotation = TRUE, values = FALSE, stopiffalse = TRUE, showwarning=FALSE)
  return(xout)
}
