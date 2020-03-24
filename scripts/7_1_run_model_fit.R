########################################################################################################
## Script to run a latent variable multi-species co-occurrence model with imperfect detection.
## Supplemental material to: 
## Tobler et al. 2019 Joint species distribution models with species correlations 
## and imperfect detection, Ecology.
########################################################################################################

### Guide to inputs names and dimensions
#n.env.var  number of random environmental variables for occupancy
#J = n.sites    number of 'sites' to fit occupancy and detection (could be separated by year/season)
#j is site index
#y          Matrix, 1 row per visit. Detection of species at a visit. The site of each visit given by ObservedSite. Values are either 0 or 1.
#ObservedSite   is a list of the site observed for each visit
#n.species  number of species     
#nlv        number of latent variables
#n          number of species
#K          for each site, the number of repititions of that site
#Xocc       matrix of covariates to predict occupancy (include a column of 1s for intercept). 1 row per site.
### Ordered according to site index: must be compatible with ObservedSite.
#Xobs       matrix of covariates to predict detection (include a column of 1s for intercept). 1 row per visit.

### within the model specification:
# mu.u.b is a list of the prior distributions for the *mean* of occupancy covariate effects (one for each covariate)
# tau.u.b is a list of the prior distributions for the standard deviation of the occupany covariate effects
# sigma.u.b is a used to define tau.u.b
# u.b is an array of distributions. Each row corresponds to a species, each column to an occupancy covariate. Distributional params given by mu.u.b and tau.u.b.
#** I didn't think this is how occupany effects would work, why are occupancy effects 'random effects' that have this two level distributions?
## In Tobler's paper 'species-level' effects are treated as random effects. To improve estimates of rare species and convergence.
## u.b combined with occupancy covariates and latent variables to give the mean of the occupancy random variable.


# mu.v.b is a list of the prior distributions for the *mean* of detection covariate effects (one for each covariate)
# tau.v.b is a list of the prior distributions for the *standard deviation* of the detection covariate effects
# sigma.v.b is a used to define tau.v.b
# v.b is an array of distributions. Each row corresponds to a species, each column to a detection covariate. Distributional params given by mu.v.b and tau.v.b.

# LV is a matrix of prior distributions for the value of latent variables. Each row corresponds to a site. Each column corresponds to a latent variable.
# lv.coef is a matrix of prior distributions of the coefficients for the LV values (restrictions really). 
## Each row corresponds to a species. Each column is a latent variable.
## Constraints are present for identifiability.

# 


library(MCMCpack)
library(mclust)
library(corrplot)

# get data 
source("./scripts/7_1_import_site_observations.R")


# detection covariates of wind, survey time, only a constant as occupancy covariate
Xocc <- model.matrix(as.formula("~ os_cover + log(ms_cover + 1)"), data = occ_covariates) #first column is intercept
Xobs <- model.matrix(as.formula("~ MeanWind + MeanTime + 1"), data = detection_data)

### Latent variable multi-species co-occurence model
modelFile='./scripts/7_1_model_description.txt'

## next is calculated just to get initial valus for occupancy covariates
y.occ.mock <- detection_data %>%
    group_by(SiteID) %>%
    summarise_at(.vars = vars(matches(species)), max) %>%
    dplyr::select(-SiteID)

#Specify the data
#Number of latent variables to use
nlv=2
n = length(detection_data_specieslist)
J <- nrow(Xocc)  #should also be max(occ_covariates$SiteID)
y <- as.matrix(detection_data[, detection_data_specieslist])
occ.data = list(n=n, J=J, y=y,
                ObservedSite = ObservedSite$SiteID,
                y.occ.mock = y.occ.mock,
                Vvisits = nrow(Xobs),
                Xocc=Xocc,Xobs=Xobs,Vocc=ncol(Xocc),Vobs=ncol(Xobs),nlv=nlv)

#Specify the parameters to be monitored
occ.params = c('z','u.b','v.b','mu.u.b','tau.u.b','mu.v.b','tau.v.b','LV','lv.coef')

#Specify the initial values
occ.inits = function() {
  lv.coef<-matrix(1,n,nlv)
  lv.coef[1:nlv,1:nlv]<-0
  for(l in 1:nlv-1){
    lv.coef[l,(l+1):nlv]<-NA
  }
  v.b.proto <- sapply(colnames(y),
                      function(x) {unname(coef(glm(((y>0)*1)[, x] ~ Xobs[, -1],  
                                                   family=binomial(link=logit))))},
                      simplify = TRUE)
  v.b <- t(v.b.proto)
  u.b.proto <- sapply(colnames(y),
                function(x) {unname(coef(glm(((y.occ.mock>0)*1)[, x] ~ Xocc[, -1],  
                                             family=binomial(link=probit))))},
                simplify = TRUE)
  u.b <- t(u.b.proto)
  list(
    #u.b=matrix(rnorm(Vocc*n),c(n,Vocc)),
    u.b= NULL,  #initial values guestimated from above are erroring! "u[14,1]: Node inconsistent with parents"
    v.b= v.b,
    u=(y.occ.mock>0)-runif(1,0.1,0.8),  #this looks strange -> step(u) is an indicator of whether occupied or not
    #mu.a = matrix(rbinom((n)*J, size=1, prob=1),
    #              nrow=J, ncol=(n)),
    #lv.coef=matrix(runif(nlv*n,-1,1),n,nlv)*lv.coef,
    LV= matrix(rnorm(nlv*J),J,nlv)
    #z = matrix(rbinom((n)*J, size=1, prob=1),
    #           nrow=J, ncol=(n))
  )
}


#run the model in JAGS with R2jags
library(R2jags)
mcmctime <- system.time(fit <- jags.parallel(occ.data, inits = occ.inits, occ.params, modelFile,
                                             n.chains=1, n.iter=3, n.burnin=1, n.thin=1))
# above took an hour on my laptop
#fit<-fit$BUGSoutput


