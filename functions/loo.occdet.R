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
#'         The difference between these two options is purely due to the latent variable value: is it drawn once for each VISIT, or once for EACH ModelSite?
#'         

# For WAIC:
## function(data_i = data[i, , drop = FALSE], draws = draws)  --> returns a vector, each entry given by draw in draws.
# data: dataframe or matrix containing predictor and observed outcome data. For each observation, i, the ith row of data will be passed to the data_i argument
#       This is like the combination of Xocc joined to Xobs and y via ModelSite?
#       Except the multiple visits to the same ModelSite are *dependent*. Perhaps it is best to combine all visits to a model site!?
# draws: a posterio draws object, passed unaltered to the function
# ...  May be used too, it is passed to each call of the function (all i).
# This function can also be used to perform the PSIS-LOO estimate of PSIS. So long as the rows satisfy conditional independence in the data model.


# Need to alter calcpredictions.R functions to 
## (1) compute probability density given latent variables are drawn from a standard Gaussian distribution
## (2) extract draws from a fit object
## (3) create the 'data' object from the fit object. Requires good naming of columns for multiple visits of a Site.