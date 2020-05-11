# Dunn-Smyth Residuals for Detection and Occupancy
# Ref: Warton Mackenzie et al, Rprsence DS residuals and Boral's Dunn-Smyth Residuals

# Inputs: object of class runjags
# Subinputs: Predictions is a dataframe with each *row* a visit and a species with the site visited given by column SiteID,
#            species given by column Species
#            and detection probabilities by pDetected.
#            Observations are a dataframe with rows the same as predictions data frame.
#            SiteID gives the site, Species indicates the species
#            'Detected' is TRUE if species detected

# Components

# given a vector (list?) of x, cdf is the CDF evaluated at x, and cdfminus is the CDF evaluated at the value just below x
cdfvals_2_dsres_discrete <- function(cdf, cdfminus, seed = NULL){
  set.seed(seed)
  u<-runif(length(cdf));  # Standard uniform value to "jitter" the cdf.
  residDet<-qnorm(cdf*u + cdfminus*(1-u));      # Dunn-Smyth residual, standard normal if cdf correct.
  return(residDet)
}

## Define a function to get the detection pdf under unequal detections, for a given site and species
## Based heavily on Rpresence code, WARNING: do not know what the license for Rpresense source code is!! Can't use code directly at least.
#' @param pDetected Is a list of the detection probabilities for all visits to the same site for the one species
#' @param x Is the value at which to evaluate the pdf
hetpdf<-function(x, pDetected)
{
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

# given detection predictions and detection observations, compute Dunn-Smyth residuals

ds_detection_residuals <- function(preds, obs, seed = NULL){
  stopifnot(isTRUE(all.equal(preds[, c("Species", "SiteID")], obs[, c("Species", "SiteID")])))
  combined <- cbind(preds, Detected = obs$Detected)
  persite <- combined %>%
    dplyr::group_by(Species, SiteID) %>%
    dplyr::summarise(numdet = sum(Detected),
                     pDetected = list(pDetected)) %>%
    dplyr::filter(numdet > 0)  # for detection residuals only use sites where a detection occured.
  # can't use mutate because hetcdf is not vectorised for the x and pDetected argument yet
  
  cdfminus <- mapply(hetcdf, x = persite$numdet - 1, pDetected = persite$pDetected, SIMPLIFY = FALSE)
  cdfminus <- unlist(cdfminus)
  pdfx <- unlist(mapply(hetpdf, x = persite$numdet, pDetected = persite$pDetected, SIMPLIFY = FALSE))
  ds_resids <- cdfvals_2_dsres_discrete(pdfx + cdfminus, cdfminus, seed = seed)
  return(ds_resids)
}

