#' @title Muffle Discarded datum Unknown warnings
#' @description Globally muffles all warnings that contain 'Discarded datum Unknown'
#' @export
global_muffle_discarddatumwarn <- function(){
  globalCallingHandlers(warning = muffle_datumwarn)
}
muffle_datumwarn <- function(w) if (any(grepl("Discarded datum [Uu]nknown", w) | grepl("Discarded datum Geocentric", w))) tryInvokeRestart("muffleWarning")
