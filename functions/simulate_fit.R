# Simulate observations from parameters
source("./functions/calcpredictions.R")

#' @examples 
#' fit <- readRDS("./tmpdata/deto_wind.rds")
#' fit$data <- as.list.format(fit$data)
#' detected <- simulate.fit(fit)
#' 
#' ## Use to check residuals
#' fit_sim <- fit
#' fit_sim$data$y <- detected
#' source("./functions/DS_residuals.R")
#' source("./functions/DS_residuals_plots.R")
#' resid_det <- ds_detection_residuals.fit(fit_sim, seed = 321)
#' resid_occ <- ds_occupancy_residuals.fit(fit_sim, seed = 123)
#' plot_residuals_detection.fit(fit_sim, 
#'                    detectionresiduals = resid_det,
#'                    varidx = 2,
#'                    plotfunction = facet_covariate) +
#' coord_cartesian(ylim = c(-1, 1))
#' plot_residuals_detection.fit(fit_sim, 
#'                    detectionresiduals = resid_det,
#'                    varidx = 2) +
#' coord_cartesian(ylim = c(-1, 1))



simulate.fit <- function(fit, paramselection = "median", conditionalLV = TRUE){
  poccupy <- poccupy_species(fit, type = paramselection, conditionalLV = conditionalLV)
  pdetectcond <- pdetect_condoccupied(fit, type = paramselection)
  fit$data <- as.list.format(fit$data)
  
  occupied <- apply(poccupy, c(1, 2), function(x) rbinom(1, 1, x))
  # array(NA, dim = c(replicates, dim(poccupy)[[1]], dim(poccupy)[[2]]),
  #       dimnames = list(replicates = paste0("replicate", 1:replicates),
  #                       modelsite = 1:nrow(poccupy),
  #                       species = colnames(poccupy)))
  
  pdetect <- pdetectcond * occupied[fit$data$ModelSite, ]
  detected <- apply(pdetect, c(1, 2), function(x) rbinom(1, 1, x))
  
  return(detected)
}
