# Dunn-Smyth Residuals for Detection and Occupancy
# Ref: Warton Mackenzie et al, Rprsence DS residuals and Boral's Dunn-Smyth Residuals

# Inputs: object of class runjags
# Subinputs: Predictions is a dataframe with each *row* a visit and a species with the site visited given by column SiteID,
#            species given by column Species
#            and detection probabilities by pDetected.
#            Observations are a dataframe with rows the same as predictions data frame.
#            SiteID gives the site, Species indicates the species
#            'Detected' is TRUE if species detected

#' @param obs Is a dataframe or matrix with each species a column, and each row a visit
#' @param fit Is a runjags object created by fitting using package runjags.
#' 

#' @examples 
#' fit <- readRDS("./tmpdata/7_1_mcmcchain_20200424.rds")
#' fit <- add.summary(fit)
#' pDetection <- probdetection_marginal(fit, type = "median")
#' colnames(pDetection) <- paste0("S", 1:ncol(pDetection))
#' detections <- list.format(fit$data)$y
#' colnames(detections) <- paste0("S", 1:ncol(detections))
#' SiteID <- list.format(fit$data)$ObservedSite
#' 
#' preds <- as_tibble(pDetection) %>% 
#'   mutate(SiteID = !!SiteID) %>%
#'   pivot_longer(-SiteID,
#'                names_to = "Species",
#'                values_to = "pDetected")
#' obs <- as_tibble(detections) %>%
#'   mutate(SiteID = !!SiteID) %>%
#'   pivot_longer(-SiteID,
#'                names_to = "Species",
#'                values_to = "Detected")
#' detection_resids <- ds_detection_residuals(preds, obs)



##### Components  ####

# given a vector (list?) of x, cdf is the CDF evaluated at x, and cdfminus is the CDF evaluated at the value just below x
cdfvals_2_dsres_discrete <- function(cdf, cdfminus, seed = NULL){
  set.seed(seed)
  u<-runif(length(cdf));  # Standard uniform value to "jitter" the cdf.
  residDet<-qnorm(cdf*u + cdfminus*(1-u));      # Dunn-Smyth residual, standard normal if cdf correct.
  return(residDet)
}

## Define a function to get the detection pdf under unequal detections, for a given site and species
## (regardless of whether species is in occupation, or if it is ever detected)
## Based heavily on Rpresence code initially (but not the rest this file) WARNING: do not know what the license for Rpresense source code is!! Can't use code directly at least.
#' @param pDetected Is a list of the detection probabilities for all visits to the same site for the one species
#' @param x Is the value at which to evaluate the pdf
hetpdf<-function(x, pDetected){
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

hetcdf <- function(x, pDetected){
  if (x < 0) {return(0)}
  x <- floor(x)
  x <- min(x, length(pDetected))
  pvals <- vapply(0:x, hetpdf, pDetected = pDetected, FUN.VALUE = 0.33)
  return(sum(pvals))
}

# given detection predictions and detection observations, compute Dunn-Smyth residuals for detection
#' @param preds is a dataframe with columns Species, SiteID, and pDetected
#' @param obs is a dataframe with columns Species, SiteID, and Detected
ds_detection_residuals <- function(preds, obs, seed = NULL){
  stopifnot(isTRUE(all.equal(preds[, c("Species", "SiteID")], obs[, c("Species", "SiteID")])))
  combined <- cbind(preds, Detected = obs$Detected)
  persite <- combined %>%
    dplyr::group_by(Species, SiteID) %>%
    dplyr::summarise(numdet = sum(Detected),
                     pDetected = list(pDetected)) %>%
    dplyr::filter(numdet > 0)  # detection residuals only use sites where a detection occured.
  # can't use mutate because hetcdf is not vectorised for the x and pDetected argument yet

  #the following are not yet conditional on detection greater than 0
  cdfminus <- unlist(mapply(hetcdf, x = persite$numdet - 1, pDetected = persite$pDetected, SIMPLIFY = FALSE))
  pdfx <- unlist(mapply(hetpdf, x = persite$numdet, pDetected = persite$pDetected, SIMPLIFY = FALSE))

  # condition on non-zero detection
  cdf0 <- unlist(mapply(hetcdf, x = 0, pDetected = persite$pDetected, SIMPLIFY = FALSE))
  cdfminus_cond <- (cdfminus - cdf0)/(1 - cdf0) #condition on non-zero detection
  pdfx_cond <- pdfx / (1 - cdf0)  #condition on non-zero detection
  
  ds_resids <- cdfvals_2_dsres_discrete(pdfx_cond + cdfminus_cond, cdfminus_cond, seed = seed)
  return(ds_resids)
}

# given occupancy predictions and detection observations, compute Dunn-Smyth residuals for occupancy
#' @param preds is a dataframe with columns Species, SiteID, and pOccupancy
#' @param obs is a dataframe with columns Species, SiteID, and Detected
ds_occupancy_residuals <- function(preds, obs, seed = NULL){
  stopifnot(isTRUE(all.equal(preds[, c("Species", "SiteID")], obs[, c("Species", "SiteID")])))
  combined <- cbind(preds, Detected = obs$Detected)
  persite <- combined %>%
    dplyr::group_by(Species, SiteID) %>%
    dplyr::summarise(anyDetected = sum(Detected) > 0,
                     pDetected = list(pDetected),
                     pOccupancy = mean(pOccupancy),  #using mean here as a shortcut --> should be all identical
                     pOccUnique = (1 == length(unique((pOccupancy)))))
  stopifnot(all(persite$pOccUnique)) #check that pOccupancy values are unique to site x species
  
  # probability of no detection
  pNoDetect <- unlist(mapply(function(pOcc, pDetected) (1 - pOcc) + pOcc * prod(1 - pDetected), 
         pOcc = persite$pOccupancy,
         pDetected = persite$pDetected,
         SIMPLIFY = FALSE))
  
  # cdf value
  persite$cdf <- 1
  persite$cdf[persite$anyDetected == FALSE] <- pNoDetect[persite$anyDetected == FALSE]
  
  # cdf minus value
  persite$cdfminus <- 0
  persite$cdfminus[persite$anyDetected == TRUE] <- pNoDetect[persite$anyDetected == TRUE]
  
  ds_resids <- cdfvals_2_dsres_discrete(persite$cdf, persite$cdfminus, seed = seed)
  return(ds_resids)
}

# simulate number of detection (replicated n times), for visit with probability of detection given by pDetected
simDetectedDistr <- function(n, pDetected){
  numdetected <- replicate(n,
            sum(vapply(pDetected, function(x) rbinom(n = 1, size = 1, prob = x), FUN.VALUE = 1)))
  return(numdetected)
}

