# Dunn-Smyth Residuals for Detection and Occupancy
# Ref: Warton Mackenzie et al, Rpresence DS residuals and Boral's Dunn-Smyth Residuals

#' @param fit Is a runjags object created by fitting using package runjags.
#' @examples 
#' fit <- readRDS("./tmpdata/7_1_mcmcchain_20200424.rds")
#' fit <- runjags::add.summary(fit)
#' source("./functions/calcpredictions.R")
#' detection_resids <- ds_detection_residuals.fit(fit, type = "median")
#' occupancy_resids <- ds_occupancy_residuals.fit(fit, type = "median")


##### Components of Detection Residual Calculations ####

#' @describeIn DunnSmythResiduals For supplied (discrete) CDF evaluations,  compute Dunn-Syth residuals.
#' @param cdf The CDF of a discrete random variable evaluated at observations of iid copies of this random variable. Must be a list or vector.
#' @param cdfminus For observations of iid copies of a discrete random variable,
#'  the CDF of the random variable evaluated at the value just below the observed values. Must be a list or vector of same length as \code{cdf}.
cdfvals_2_dsres_discrete <- function(cdf, cdfminus, seed = NULL){
  set.seed(seed)
  u<-runif(length(cdf));  # Standard uniform value to "jitter" the cdf.
  residDet<-qnorm(cdf*u + cdfminus*(1-u));      # Dunn-Smyth residual, standard normal if cdf correct.
  return(residDet)
}

#' @describeIn DunnSmythResiduals For a given ModelSite and species,
#'  computes the (discrete) probability density function (pdf) for the number of detections of the species at the ModelSite.
#' Based heavily on Rpresence code for hetpdf() initially (but not the rest this file) WARNING: do not know what the license for Rpresense source code is!! Can't use code directly at least.
#' @param pDetected Is a list of the detection probabilities for each visit to the ModelSite for a single species
#' @param x Is the value at which to evaluate the pdf
numdet_pdf<-function(x, pDetected){
  p0 <- prod(1 - pDetected) #probability of no detections
  if (x == 0){return(p0)}
  if ((x < 0) | (x > length(pDetected))) {return(0)} #x outside support
  if ((abs(as.integer(x) - x)) > 1E-8){return(0)} #x not an integer
  ind<-combn(length(pDetected), x) #index of visits that the species could be detected at
  
  tmp_pdens <- matrix(pDetected[as.vector(ind)], byrow = FALSE, nrow = x)
  pdends_ind <- apply(tmp_pdens / (1 - tmp_pdens), 2, prod) *  #the detections in numerator and denominator because p0 used for non-detections
    p0

  return(sum(pdends_ind));
}

#' @describeIn DunnSmythResiduals For a given ModelSite and species,
#'  computes the (discrete) cumulative distribution function (cdf) for the number of detections of the species at the ModelSite.
#' Based heavily on Rpresence code for numdet_cdf initially (but not the rest this file) WARNING: do not know what the license for Rpresense source code is!! Can't use code directly at least.
#' @param pDetected Is a list of the detection probabilities for each visit to the ModelSite for a single species
#' @param x Is the value at which to evaluate the pdf
numdet_cdf <- function(x, pDetected){
  if (x < 0) {return(0)}
  x <- floor(x)
  x <- min(x, length(pDetected))
  pvals <- vapply(0:x, numdet_pdf, pDetected = pDetected, FUN.VALUE = 0.33)
  return(sum(pvals))
}

##### Full Dunn-Smyth Residual Functions ####
#' @describeIn DunnSmythResiduals Given a fitted occupancy detection model (variable names must match). Computes Dunn-Smyth residuals for detection, marginalising the latent variables.
#' @param fit A fitted occupancy-detection model.
#' @param seed A seed to fix randomness of Dunn-Smyth residual jitter.
#' @param type The type of point estimate to use for parameter estimates. See \code{\link{pdetect_indspeciesvisit_LVmarginal}}
#' @value A matrix, each row is a ModelSite and each column is a species.
#' Detection residuals are only computed for species and sites that have at least one detection. Other values are NA.
ds_detection_residuals.fit <- function(fit, type = "median", seed = NULL, conditionalLV = TRUE){
  if (conditionalLV) {pDetection <- pdetect_indvisit_condLV(fit, type = type)}  #the detection probabilities
  else {pDetection <- pdetect_indspeciesvisit_LVmarginal(fit, type = type)}  #the detection probabilities
  colnames(pDetection) <- paste0("S", 1:ncol(pDetection)) #name the species S1....Sn
  fitdata <- list.format(fit$data)
  detections <-  fitdata$y
  colnames(detections) <- paste0("S", 1:ncol(detections))
  if ("ObservedSite" %in% names(fitdata)){ModelSite <- fitdata$ObservedSite} #to enable calculation on the early fitted objects with different name
  if ("ModelSite" %in% names(fitdata)){ModelSite <- fitdata$ModelSite}

  # Convert the above into format suitable for ds_detection_residuals.raw
  preds <- as_tibble(pDetection) %>%
    mutate(ModelSite = !!ModelSite) %>%
    pivot_longer(-ModelSite,
                 names_to = "Species",
                 values_to = "pDetected")
  obs <- as_tibble(detections) %>%
    mutate(ModelSite = !!ModelSite) %>%
    pivot_longer(-ModelSite,
                 names_to = "Species",
                 values_to = "Detected")
  
  # Compute residuals
  detection_resids <- ds_detection_residuals.raw(preds, obs, seed = seed)
  detection_resids %>%
    pivot_wider(names_from = "Species",
                values_from = "DetectionResidual") %>%
    return()
}

 
#' @describeIn DunnSmythResiduals Given predictions for detection probability, and corresponding and detection observations, compute Dunn-Smyth residuals for detection
#' @param preds is a dataframe with columns Species, ModelSite, and pDetected
#' @param obs is a dataframe with columns Species, ModelSite, and Detected
#' @value A dataframe with a columns for Species, ModelSite, and detection residual. 
#' The residual is only computed for species detected at least once at a site.
ds_detection_residuals.raw <- function(preds, obs, seed = NULL){
  stopifnot(all(c("Species", "ModelSite", "pDetected") %in% names(preds)))
  stopifnot(all(c("Species", "ModelSite", "Detected") %in% names(obs)))
  stopifnot(isTRUE(all.equal(preds[, c("Species", "ModelSite")], obs[, c("Species", "ModelSite")])))
  combined <- cbind(preds, Detected = obs$Detected)
  persite <- combined %>%
    dplyr::group_by(Species, ModelSite) %>%
    dplyr::summarise(numdet = sum(Detected),
                     pDetected = list(pDetected)) %>%
    dplyr::filter(numdet > 0)  # detection residuals only use sites where a detection occured.
  # can't use mutate because numdet_cdf is not vectorised for the x and pDetected argument yet
  stopifnot(nrow(persite) > 0) #means there is no detection residuals to compute

  #the following are not yet conditional on detection greater than 0
  cdfminus <- unlist(mapply(numdet_cdf, x = persite$numdet - 1, pDetected = persite$pDetected, SIMPLIFY = FALSE))
  pdfx <- unlist(mapply(numdet_pdf, x = persite$numdet, pDetected = persite$pDetected, SIMPLIFY = FALSE))

  # condition on non-zero detection
  cdf0 <- unlist(mapply(numdet_cdf, x = 0, pDetected = persite$pDetected, SIMPLIFY = FALSE))
  cdfminus_cond <- (cdfminus - cdf0)/(1 - cdf0) #condition on non-zero detection
  pdfx_cond <- pdfx / (1 - cdf0)  #condition on non-zero detection
  
  ds_resids <- cdfvals_2_dsres_discrete(pdfx_cond + cdfminus_cond, cdfminus_cond, seed = seed)
  return(data.frame(Species = persite$Species, ModelSite = persite$ModelSite, DetectionResidual = ds_resids))
}

#' @describeIn DunnSmythResiduals Given a fitted occupancy detection model (variable names must match).
#'  Computes Dunn-Smyth residuals for occupancy, marginalising the latent variables.
#' @param fit A fitted occupancy-detection model.
#' @param seed A seed to fix randomness of Dunn-Smyth residual jitter.
#' @param type The type of point estimate to use for parameter estimates. See \code{\link{pdetect_indspeciesvisit_LVmarginal}}
#' @value A matrix, each row is a ModelSite and each column is a species.
ds_occupancy_residuals.fit <- function(fit, type = "median", seed = NULL, conditionalLV = TRUE){
  if (conditionalLV){pOccupancy <- poccupy_species_condLV(fit, type = type)} #occupany probabilities
  else {pOccupancy <- poccupy_indspecies_LVmarginal(fit, type = type)} #occupany probabilities
  colnames(pOccupancy) <- paste0("S", 1:ncol(pOccupancy)) #name the species S1....Sn
  pDetected_cond <- pdetect_condoccupied(fit, type = type)  #the detection probabilities if sites occupied
  colnames(pDetected_cond) <- paste0("S", 1:ncol(pDetected_cond)) #name the species S1....Sn
  fitdata <- list.format(fit$data)
  detections <- fitdata$y
  colnames(detections) <- paste0("S", 1:ncol(detections))
  if ("ObservedSite" %in% names(fitdata)){ModelSite <- fitdata$ObservedSite} #to enable calculation on the early fitted objects with different name
  if ("ModelSite" %in% names(fitdata)){ModelSite <- fitdata$ModelSite}
  
  # convert to format for raw function
  preds.occ <- as_tibble(pOccupancy) %>%
    rowid_to_column(var = "ModelSite") %>% #because each row of Xobs is a model site
    pivot_longer(-ModelSite,
                 names_to = "Species",
                 values_to = "pOccupancy")
  preds.det <- as_tibble(pDetected_cond) %>%
    mutate(ModelSite = !!ModelSite) %>%
    pivot_longer(-ModelSite,
                 names_to = "Species",
                 values_to = "pDetected_cond")
  preds <- inner_join(preds.occ, preds.det, by = c("Species", "ModelSite"))
  obs <- as_tibble(detections) %>%
    mutate(ModelSite = !!ModelSite) %>%
    pivot_longer(-ModelSite,
                 names_to = "Species",
                 values_to = "Detected")
  
  # apply raw occupancy residuals function
  residuals <- ds_occupancy_residuals.raw(preds, obs)
  residuals %>%
    pivot_wider(names_from = "Species",
                values_from = "OccupancyResidual") %>%
    return()
}

# given occupancy predictions and detection observations, compute Dunn-Smyth residuals for occupancy
#' @param preds is a dataframe with columns Species, ModelSite, pOccupancy, and pDetected_cond.
#'  pOccupancy is the probability of ModelSite being occupied.
#'  pDetected_cond is the probability of detecting the species, given the species occupies the ModelSite.
#' @param obs is a dataframe with columns Species, ModelSite, and Detected
ds_occupancy_residuals.raw <- function(preds, obs, seed = NULL){
  stopifnot(all(c("Species", "ModelSite", "pOccupancy", "pDetected_cond") %in% names(preds)))
  stopifnot(all(c("Species", "ModelSite", "Detected") %in% names(obs)))
  stopifnot(isTRUE(all.equal(preds[, c("Species", "ModelSite")], obs[, c("Species", "ModelSite")])))
  combined <- cbind(preds, Detected = obs$Detected)
  persite <- combined %>%
    dplyr::group_by(Species, ModelSite) %>%
    dplyr::summarise(anyDetected = sum(Detected) > 0,
                     pDetected_cond = list(pDetected_cond),
                     pOccupancy = first(pOccupancy))  #using first here as a shortcut --> should be all identical
                     # pOccUnique = (1 == length(unique((pOccupancy)))))  # a check that pOccupancy unique, removed for speed
  # stopifnot(all(persite$pOccUnique)) #check that pOccupancy values are unique to site x species
  
  # probability of no detection
  pNoDetect <- unlist(mapply(function(pOcc, pDetected_cond) (1 - pOcc) + pOcc * prod(1 - pDetected_cond), 
         pOcc = persite$pOccupancy,
         pDetected_cond = persite$pDetected_cond,
         SIMPLIFY = FALSE))
  
  # cdf value
  persite$cdf <- 1
  persite$cdf[persite$anyDetected == FALSE] <- pNoDetect[persite$anyDetected == FALSE]
  
  # cdf minus value
  persite$cdfminus <- 0
  persite$cdfminus[persite$anyDetected == TRUE] <- pNoDetect[persite$anyDetected == TRUE]
  
  ds_resids <- cdfvals_2_dsres_discrete(persite$cdf, persite$cdfminus, seed = seed)
  return(data.frame(Species = persite$Species, ModelSite = persite$ModelSite, OccupancyResidual = ds_resids))
}


#### Extra Functions ####
# simulate number of detection (replicated n times), for visit with probability of detection given by pDetected
simDetectedDistr <- function(n, pDetected){
  numdetected <- replicate(n,
            sum(vapply(pDetected, function(x) rbinom(n = 1, size = 1, prob = x), FUN.VALUE = 1)))
  return(numdetected)
}

