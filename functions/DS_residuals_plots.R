## DS_residual_plotting

# two situations:
# 1) ploting residuals against a covariate already fitted  in the model
# 2) plotting residauls against a covariate that is not yet included in the model

library(ggplot2); library(dplyr);
#' @param fit Is a runjags object created by fitting using package runjags.
#' @examples 
#' fit <- readRDS("./tmpdata/7_1_mcmcchain_20200424.rds")
#' fit <- runjags::add.summary(fit)
#' source("./functions/calcpredictions.R")
#' source("./functions/DS_residuals.R")
#' detection_resids <- ds_detection_residuals.fit(fit, type = "median")
#' plot_residuals_detection.fit(fit, detection_resids, varidx = 2)
#' plot_residuals_detection.fit(fit, varidx = 2, esttype = "median")
#' plot_residuals_detection.fit(fit, varidx = 4, esttype = "median")
#' 
#' plot_residuals_occupancy.fit(fit, varidx = 3, esttype = "median")
#' plot_residuals_occupancy.fit(fit, varidx = c(2, 3, 4), esttype = "median")
#' 
#' # Residuals against an unincluded covariate:
#' source("./scripts/7_1_import_site_observations.R")
#' covar <- occ_covariates[ , "ms_cover", drop = FALSE] %>%
#'   rowid_to_column(var = "ModelSite")
#' residuals <- ds_occupancy_residuals.fit(fit, type = "median")
#' pltobj <- plot_residuals.residual(residuals, covar, plot = FALSE)
#' pltobj + scale_y_continuous(name = "Occupancy Residuals")

#' @describeIn plot_residuals For table of provided residuals and covariate values, makes residual plots.
#' Residual and covariate values must be provided with the ModelSite.
#' @param residuals is a table returned by \code{ds_occupancy_residuals.fit} or \code{ds_detection_residuals.fit}. Each row is a unique ModelSite.
#' It must have a column named 'ModelSite', all other columns are assumed contain residuals, and the column name corresponds to species names.
#' @param covar Is a table of covariate values, it must have a column labelled 'ModelSite' that gives the ModelSite of covariate value.
#' Rows with duplicated ModelSite values are averaged, which is in keeping with Warton et al for detection residuals.
plot_residuals.residual <- function(residuals, covar, plot = TRUE){
  # Average covariates to ModelSite level. This is what Warton, Mackenzie et al do. In future it could be possible to present residuals per visit
  if (anyDuplicated(covar[, "ModelSite"]) > 0){
    covar <- covar %>%
      as_tibble() %>%
      group_by(ModelSite) %>%
      summarise_all(mean)
  }
  
  # prepare species names from input residuals
  speciesnames <- setdiff(colnames(residuals), "ModelSite")
  speciesnames <- speciesnames[order(as.integer(gsub("^S", "", speciesnames)))]  #order the species by index
  
  # prepare covarnames
  covarnames <- setdiff(colnames(covar), "ModelSite")
  
  # prepare to print
  outframe <- left_join(residuals, covar, by = "ModelSite") %>%
    pivot_longer(any_of(speciesnames),
                 names_to = "Species",
                 values_to = "Residual",
                 values_drop_na = TRUE) %>%
    mutate(Species = ordered(Species, levels = speciesnames)) #to order species according to 'speciesnames'
  outframe <- outframe %>%
    pivot_longer(any_of(covarnames),
                 names_to = "Covariate",
                 values_to = "CovarValue") 
  pltobject <- outframe %>% 
    ggplot() +
    facet_wrap(~ Covariate + Species, as.table = TRUE) +
    geom_hline(yintercept = 0, col = "grey", lty = "dashed") +
    geom_point(aes(x = CovarValue, y = Residual)) +
    geom_smooth(aes(x = CovarValue, y = Residual), method = "gam", level = 0.95, formula = y ~ s(x, bs = "cs")) 
  if (plot){
    print(pltobject)
  }
  invisible(pltobject)
}


#' @describeIn ?? Prepares tibbles and plots of residuals for covariates that are part of a fitted object
#' @param fit The fitted runjags object.
#' @param varidx The index of the covariate to plot. If NULL, then all covariates will be plotted
#' @param detectionresiduals Optional. A tibble of already calculated detection residuals.
#' Must have column names identical to output of \code{ds_detection_residuals.fit}.
#' If not supplied then detection residuals are computed from \code{fit} using \code{ds_detection_residuals.fit}.
#' @param esttype The point estimate extracted from fit. Passed to \code{ds_detection_residuals.fit} as argument \code{type}.
#' @param plot Defaults to TRUE. If false return the ggplot object without plotting it.
#' @value A ggplot object. Data is saved in the \code{data} slot.
plot_residuals_detection.fit <- function(fit, detectionresiduals = NULL, varidx = NULL, esttype = NULL, plot = TRUE){
  stopifnot(is.null(detectionresiduals) | is.null(esttype))  #error if est type is supplied when detection residuals is also supplied
  fitdata <- list.format(fit$data)
  if (is.null(detectionresiduals)) {detectionresiduals <- ds_detection_residuals.fit(fit, type = esttype)}
  
  # get detection covariates
  detectioncovars <- fitdata$Xobs
  colnames(detectioncovars)[1:fitdata$Vobs] <- paste0("Xobs", 1:fitdata$Vobs)
  if (!is.null(varidx)){detectioncovars <- detectioncovars[, varidx, drop = FALSE]}  
  detectioncovars <- cbind(detectioncovars, ModelSite = fitdata$ObservedSite)
 
  pltobject <- plot_residuals.residual(detectionresiduals, detectioncovars, plot = FALSE) +
      scale_y_continuous(name = "Detection Residual")
  if (plot){
    print(pltobject)
  }
  invisible(pltobject)
}

#' @describeIn ?? Prepares tibbles and plots of occupancy residuals for covariates that are part of a fitted object
#' @param fit The fitted runjags object.
#' @param varidx The index of the covariate to plot. If NULL, then all occupancy covariates will be plotted
#' @param occupancyresiduals Optional. A tibble of already calculated occupancy residuals.
#' Must have column names identical to output of \code{ds_detection_residuals.fit}.
#' If not supplied then detection residuals are computed from \code{fit} using \code{ds_detection_residuals.fit}.
#' @param esttype The point estimate extracted from fit. Passed to \code{ds_detection_residuals.fit} as argument \code{type}.
#' @param plot Defaults to TRUE. If false return the ggplot object without plotting it.
#' @value A ggplot object. Data is saved in the \code{data} slot.
plot_residuals_occupancy.fit <- function(fit, occupancyresidual = NULL, varidx = NULL, esttype = NULL, plot = TRUE){
  stopifnot(is.null(occupancyresidual) | is.null(esttype))  #error if est type is supplied when detection residuals is also supplied
  fitdata <- list.format(fit$data)
  if (is.null(occupancyresidual)) {occupancyresidual <- ds_occupancy_residuals.fit(fit, type = esttype)}
  
  # get occupancy covariates
  occupancycovars <- fitdata$Xocc
  colnames(occupancycovars)[1:fitdata$Vocc] <- paste0("Xocc", 1:fitdata$Vocc)
  if (!is.null(varidx)){occupancycovars <- occupancycovars[, varidx, drop = FALSE]}  
  occupancycovars <- occupancycovars %>% as_tibble() %>% rowid_to_column(var = "ModelSite")
  
  pltobject <- plot_residuals.residual(occupancyresidual, occupancycovars, plot = FALSE) +
      scale_y_continuous(name = "Occupancy Residual")
  if (plot){
    print(pltobject)
  }
  invisible(pltobject)
}
