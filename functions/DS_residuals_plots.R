## DS_residual_plotting

# two situations:
# 1) ploting residuals against a covariate already fitted  in the model
# 2) plotting residauls against a covariate that is not yet included in the model

library(ggplot2); library(dplyr);
#' @param fit Is a runjags object created by fitting using package runjags.
#' @examples 
#' fit <- readRDS("./tmpdata/7_1_mcmcchain_20200424.rds")
#' fit <- runjags::add.summary(fit)
#' detection_resids <- ds_detection_residuals.fit(fit, type = "median")
#' plot_detection_residual.fit(fit, detection_resids, varidx = 2)
#' plot_detection_residual.fit(fit, varidx = 2, esttype = "median")
#' plot_detection_residual.fit(fit, varidx = 3, esttype = "median")

fitdata <- list.format(fit$data)
OSCanopy <- as_tibble(fitdata$Xocc[, 2]) %>% rowid_to_column(var = "ModelSite")
covariatetable <- OSCanopy
residualtable <- detection_resids
speciesnames <- setdiff(names(residualtable), "ModelSite")
speciesnames <- speciesnames[order(as.integer(gsub("S", "", speciesnames)))]  #order the species by index

df <- covariatetable %>%
  group_by(ModelSite) %>%
  summarise_all(mean, na.rm = TRUE) %>%
  inner_join(residualtable, by = "ModelSite", suffix = c(".X", ""))

df %>%
  pivot_longer(speciesnames,
               names_to = "Species",
               values_to = "Residual",
               values_drop_na = TRUE) %>%
  mutate(Species = ordered(Species, levels = speciesnames)) %>% #to order species according to 'speciesnames'
  ggplot() +
  facet_wrap(~Species, as.table = TRUE) +
  geom_hline(yintercept = 0) +
  geom_point(aes(x = value, y = Residual)) +
  geom_smooth(aes(x = value, y = Residual), method = "gam", level = 0.95, formula = y ~ s(x, bs = "cs"))

#' @describeIn ?? Prepares tibbles and plots of residuals for covariates that are part of a fitted object
#' @param fit The fitted runjags object.
#' @param varidx The index of the covariate to plot. If NULL, then all covariates will be plotted
#' @param detectionresiduals Optional. A tibble of already calculated detection residuals.
#' Must have column names identical to output of \code{ds_detection_residuals.fit}.
#' If not supplied then detection residuals are computed from \code{fit} using \code{ds_detection_residuals.fit}.
#' @param esttype The point estimate extracted from fit. Passed to \code{ds_detection_residuals.fit} as argument \code{type}.
#' @param plot Defaults to TRUE. If false return the ggplot object without plotting it.
#' @value A ggplot object. Data is saved in the \code{data} slot.
plot_detection_residual.fit <- function(fit, detectionresiduals = NULL, varidx = NULL, esttype = NULL, plot = TRUE){
  stopifnot(is.null(detectionresiduals) | is.null(esttype))  #error if est type is supplied when detection residuals is also supplied
  fitdata <- list.format(fit$data)
  if (is.null(detectionresiduals)) {detectionresiduals <- ds_detection_residuals.fit(fit, type = esttype)}
  speciesnames <- setdiff(names(detectionresiduals), "ModelSite")
  speciesnames <- speciesnames[order(as.integer(gsub("^S", "", speciesnames)))]  #order the species by index
  
  # get detection covariates
  detectioncovars <- fitdata$Xobs
  colnames(detectioncovars)[1:fitdata$Vobs] <- paste0("Xobs", 1:fitdata$Vobs)
  if (!is.null(varidx)){detectioncovars <- detectioncovars[, varidx, drop = FALSE]}  
  detectioncovars <- cbind(detectioncovars, ModelSite = fitdata$ObservedSite)
  
  # Average detection covariates to ModelSite level. This is what Warton, Mackenzie et al do. In future it could be possible to present residuals per visit
  detectioncovars_ModelSite <- detectioncovars %>%
    as_tibble() %>%
    group_by(ModelSite) %>%
    summarise_all(mean)
  covarnames <- setdiff(names(detectioncovars_ModelSite), "ModelSite")
  
  outframe <- left_join(detectionresiduals, detectioncovars_ModelSite, by = "ModelSite") %>%
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

#' @param residualtable is a dataframe with first column ModelSite, and second column the residual (this works for both occupancy and detection residuals)
#' @param covariatetable is a dataframe with first column ModelSite and other column's covariate values.
plot.dsresidual <- function(residualtable, covariatetable){
  
}

prep_residualdf <- function(residualtable, covariatetable){
  
}