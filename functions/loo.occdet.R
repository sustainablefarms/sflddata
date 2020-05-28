## Computing WAIC (and possibly PSIS-LOO) with the help of the LOO package by Vehtari and Gelman

#' @details Any predictinve accuracy measure requires a choice of (1) the part of the model that is considered the 'likelihood' and (2) factorisation of the likelihood into 'data points' [Vehtari 2017]
#' On (1): New data will look like a new location or visit for a new season in our exisitng region, and only the species included in the model.
#' This means we have zero knowledge of the latent variable value at the new ModelSite.
#'         This means likelihood:
#'          (a) conditional on the covariates u.b and v.b (not using the fitted values of mu.u.b, tau.u.b etc)
#'          (b) is conditional on the lv.coef values of each species
#'          (c) is conditional on the latent variable value for (each) new ModelSite being drawn from a standard Gaussian distribution? **ask Wade**
#' On (2): Factoring the likelihood using the inbuilt independence properties corresponds to a 'point' being all data for all visits for a single ModelSite.
#'         The likelihood could also be partitioned conditional on occupancy, i.e. each visit is considered a data point.
#'         What is the difference between these methods?      

# For WAIC:
## function(data_i = data[i, , drop = FALSE], draws = draws)  --> returns a vector, each entry given by draw in draws.
# data: dataframe or matrix containing predictor and observed outcome data. For each observation, i, the ith row of data will be passed to the data_i argument
#       This is like the combination of Xocc joined to Xobs and y via ModelSite?
#       Except the multiple visits to the same ModelSite are *dependent*. Perhaps it is best to combine all visits to a model site!?
# draws: a posterior draws object, passed unaltered to the function
# ...  May be used too, it is passed to each call of the function (all i).
# This function can also be used to perform the PSIS-LOO estimate of PSIS. So long as the rows satisfy conditional independence in the data model.


# Need to alter calcpredictions.R functions to 
## (1) compute joint probability of species occupancy given latent variables are drawn from a standard Gaussian distribution 
##     (currently calcprobabilities treats the occupancy of other species as irrelevant)
##     The boral package has a 'calc.marglogLik' function that does this using 1000 simulations of the latent variable values.
##     Given each LV draw, the detection of each species is independent, and for each model site can be quickly calculated.
##     The results can be multiplied for the joint probability density given the LV draw.
##     After this, the average across LV draws will give the full, marginal, joint probability density.
## (2) extract draws from a fit object
## (3) create the 'data' object from the fit object. Requires use of unusual element data types, and good naming of columns for multiple visits of a Site.

#' @examples
#' source("./functions/calcpredictions.R")
#' fit <- readRDS("./tmpdata/7_1_mcmcchain_20200424.rds")
#' fitdata <- list.format(fit$data)
#' library(dplyr); library(tidyr); library(tibble);
#' Xocc <- fitdata$Xocc %>%
#'   as_tibble() %>%
#'   rowid_to_column(var = "ModelSite") %>%
#'   nest(Xocc = -ModelSite)
#' Xobs <- fitdata$Xobs %>%
#'   as_tibble() %>%
#'   mutate(ModelSite = fitdata$ObservedSite) %>%
#'   nest(Xobs = -ModelSite)
#' y <- fitdata$y %>%
#'   as_tibble() %>%
#'   mutate(ModelSite = fitdata$ObservedSite) %>%
#'   nest(y = -ModelSite)
#' data <- inner_join(Xocc, Xobs, by = "ModelSite", suffix = c("occ", "obs")) %>%
#'   inner_join(y, by = "ModelSite", suffix = c("X", "y"))
#' nlv <- 2
#' numsims <- 1000
#' lvsim <- matrix(rnorm(nlv * numsims), ncol = 2, nrow = numsims) #simulated lv values, should average over thousands
#' draws <- fit$mcmc[[1]]
#' cl <- parallel::makeCluster(1)
# profvis::profvis({pdetect_joint_marginal.data_i(data_i = data[1, , drop = FALSE], draws[1:10, ], lvsim)})
# profvis::profvis({pdetect_joint_marginal.ModelSite(data[1, "Xocc", drop = TRUE][[1]],
#                                  data[1, "Xobs", drop = TRUE][[1]],
#                                  data[1, "y", drop = TRUE][[1]],
#                                  draws[1, ],
#                                  lvsim
#                                  )})


# library(loo)
waic <- loo::waic(pdetect_joint_marginal.data_i,
                  data = data[1:10, ],
                  draws = draws[1:10, ],
                  lvsim = lvsim)
# Above took 10 000 milliseconds on first go.
# After bugsvar2array faster, took 8000ms. Could pool bugsvar2array work to be even faster.
# After avoiding all dataframe use, dropped to 3000ms
# Can do all of JointSpVst_Liklhood.LV()'s work as matrix manipulations, dropped to 1800ms
# Replaced use of "rep" with Rfast's functions eachrow, and also replaced row product with Rfast::rowprod. Down to 860ms.


#' looest <- loo::loo(pdetect_joint_marginal.data_i,
#'                    data = data,
#'                    draws = draws,
#'                    lvsim = lvsim,
#'                    cl = cl)
#' parallel::stopCluster(cl)

#' @param draws A large matrix. Each column is a model parameter, with array elements named according to the BUGS naming convention.
#' Each row of \code{draws} is a simulation from the posterior.
#' @param data_i A row of a data frame containing data for a single ModelSite. 
#' @param lvsim A matrix of simulated LV values. Columns correspond to latent variables, each row is a simulation
pdetect_joint_marginal.data_i <- function(data_i, draws, lvsim, cl = NULL){
  Xocc <- data_i[, "Xocc", drop = TRUE][[1]]
  Xobs <- data_i[, "Xobs", drop = TRUE][[1]]
  y <- data_i[, "y", drop = TRUE][[1]]
  
  if (is.null(cl)){
    Likl_margLV <- apply(draws, 1, function(theta) pdetect_joint_marginal.ModelSite(
      Xocc, Xobs, y, theta, lvsim))
  } else {
    parallel::clusterExport(cl, list("pdetect_joint_marginal.ModelSite",
                           "JointSpVst_Liklhood.LV",
                           "bugsvar2array",
                           "Xocc", "Xobs", "y", "lvsim"
                           ))
    parallel::clusterEvalQ(cl, library(dplyr))
    Likl_margLV <- parallel::parApply(cl, draws, 1, function(theta) pdetect_joint_marginal.ModelSite(
      Xocc, Xobs, y, theta, lvsim))
  }
  return(log(Likl_margLV))
}

#' @param Xocc A matrix of occupancy covariates. Must have a single row. Columns correspond to covariates.
#' @param Xobs A matrix of detection covariates, each row is a visit.
#' @param y A matrix of detection data for a given model site. 1 corresponds to detected. Each row is visit, each column is a species.
#' @param theta A vector of model parameters, labelled according to the BUGS labelling convention seen in runjags
#' @param lvsim A matrix of simulated LV values. Columns correspond to latent variables, each row is a simulation
pdetect_joint_marginal.ModelSite <- function(Xocc, Xobs, y, theta, lvsim){
  stopifnot(nrow(Xocc) == 1)
  stopifnot(nrow(Xobs) == nrow(y))
  y <- as.matrix(y)
  Xocc <- as.matrix(Xocc)
  Xobs <- as.matrix(Xobs)
  u.b <- bugsvar2array(theta, "u.b", 1:ncol(y), 1:ncol(Xocc))[,,1]  # rows are species, columns are occupancy covariates
  v.b <- bugsvar2array(theta, "v.b", 1:ncol(y), 1:ncol(Xobs))[,,1]  # rows are species, columns are observation covariates
  lv.coef <- bugsvar2array(theta, "lv.coef", 1:ncol(y), 1:ncol(lvsim))[,,1] # rows are species, columns are lv
  sd_u_condlv <- sqrt(1 - rowSums(lv.coef^2)) #for each species the standard deviation of the indicator random variable 'u', conditional on values of LV

  ## Probability of Detection, CONDITIONAL on occupied
  Detection.LinPred <- as.matrix(Xobs) %*% t(v.b)
  Detection.Pred.Cond <- exp(Detection.LinPred) / (exp(Detection.LinPred) + 1)   #this is the inverse logit function

  
  ## Likelihood (probability) of single visit given occupied
  Likl_condoccupied <- Detection.Pred.Cond * y + (1 - Detection.Pred.Cond) * (1 - y) # non-detection is complement of detection probability
  # Likl_condoccupied[y == 0] <- (1 - Detection.Pred.Cond)[y == 0]   # non-detection is complement of detection probability
  
  ## Joint likelihood (probability) of detections of all visits CONDITIONAL on occupied
  Likl_condoccupied.JointVisit <- apply(Likl_condoccupied, 2, prod)
  
  ## Likelihood (proability) of y given unoccupied is either 1 or 0 for detections. Won't include that here yet.
  NoneDetected <- as.numeric(colSums(y) == 0)

  ## Probability of Site Occupancy
  ModelSite.Occ.eta_external <- as.matrix(Xocc) %*% t(u.b) #columns are species
  
  # probability of occupancy given LV
  ModelSite.Occ.eta_LV <- lvsim %*% t(lv.coef) #occupancy contribution from latent variables, performed all together
  ModelSite.Occ.eta <- Rfast::eachrow(ModelSite.Occ.eta_LV, ModelSite.Occ.eta_external, oper = "+") #add the external part to each simulation
  ModelSite.Occ.Pred.CondLV <- 1 - pnorm(- ModelSite.Occ.eta,
                                  mean = 0,
                                  sd = sd_u_condlv)  #standard deviation isn't 1 when given LVs
  
  # likelihood given LV
  Likl.JointVisit.condLV <- Rfast::eachrow(ModelSite.Occ.Pred.CondLV, Likl_condoccupied.JointVisit, oper = "*") #per species likelihood, occupied component. Works because species conditionally independent given LV
  Likl.JointVisit.condLV <- Likl.JointVisit.condLV + 
    Rfast::eachrow((1 - ModelSite.Occ.Pred.CondLV), NoneDetected, oper = "*") #add probability of unoccupied for zero detections
  
  # combine with likelihoods of detections
  Likl.JointVisitSp.condLV <- Rfast::rowprods(Likl.JointVisit.condLV)  # multiply probabilities of each species together because species are conditionally independent

  # take mean of all LV sims to get likelihood marginalised across LV values
  Likl_margLV <- mean(Likl.JointVisitSp.condLV)
  
  return(Likl_margLV)
}


#' @description Given latent variable values, compute the joint probability of all visit and species detections (for a given model site)
#' @param LVval A single set of latent variable values - a matrix with 1 row, and nlv columns.
#' @param lv.coef The loadings of the latent variables. Row correspond to species, columns to latent variables.
#' @param Likl_condoccupied.JointVisit For each species the likelihood of the supplied observations (joint across all visits to the ModelSite),
#'  assuming that each species is in occupation.
#' @param ModelSite.Occ.LinPred_external The contribution to the occupancy linear predictor given by the external covariates.
#' @param NoneDetected A boolean vector indicating which species was not detected in any visit to the ModelSite
JointSpVst_Liklhood.LV <- function(LVval,
                                   lv.coef, 
                                   sd_u_condlv,
                                   Likl_condoccupied.JointVisit,
                                   ModelSite.Occ.LinPred_external,
                                   NoneDetected){
  
  
  return(Likl.JointVisitSp.condLV)
}
  