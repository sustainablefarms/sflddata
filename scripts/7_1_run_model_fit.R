########################################################################################################
## Script to run a latent variable multi-species co-occurrence model with imperfect detection.
## Supplemental material to: 
## Tobler et al. 2019 Joint species distribution models with species correlations 
## and imperfect detection, Ecology.
########################################################################################################

### Guide to inputs names and dimensions
#n.env.var  number of random environmental variables for occupancy
#J = n.sites    number of 'sites' to fit occupancy and detection (is separated by year/season)
#               (j is often used for the site index)
#y          Matrix, 1 row per visit. Detection of species at a visit. The site of each visit given by ObservedSite. Values are either 0 or 1.
#ObservedSite   is a list of the site observed for each visit
#n.species  number of species     
#nlv        number of latent variables
#n          number of species
#Xocc       matrix of covariates to predict occupancy (include a column of 1s for intercept). 1 row per site.
#           (Ordered according to site index: must be compatible with ObservedSite.)
#Xobs       matrix of covariates to predict detection (include a column of 1s for intercept). 1 row per visit.
#Vvisits    number of visits in total

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
Xocc <- model.matrix(as.formula("~ os_cover + log(ms_cover + 1) + NMdetected"), data = occ_covariates) #first column is intercept
Xocc[, -1] <- scale(Xocc[, -1])
Xobs <- model.matrix(as.formula("~ MeanWind + MeanTime + 1"), data = detection_data)
Xobs[, -1] <- scale(Xobs[, -1])

### Latent variable multi-species co-occurence model
modelFile='./scripts/7_1_model_description.txt'

## next is calculated just to get initial values for occupancy covariates
y.occ.mock <- detection_data %>%
  group_by(SiteID) %>%
  summarise_at(.vars = vars(matches(species)), max) %>%
  dplyr::select(-SiteID)

#Specify the data
nlv = 2 #Number of latent variables to use
n = length(detection_data_specieslist) #number of species
J <- nrow(Xocc)  #number of unique sites should also be max(occ_covariates$SiteID)
y <- as.matrix(detection_data[, detection_data_specieslist])
data.list = list(n=n, J=J, y=y,
                ObservedSite = detection_data$SiteID, #a list of the site visited at each visit
                y.occ.mock = y.occ.mock,
                Vvisits = nrow(Xobs), #number of visits in total - not sure what this is for
                Xocc=Xocc,Xobs=Xobs,Vocc=ncol(Xocc),Vobs=ncol(Xobs),nlv=nlv)

#Specify the parameters to be monitored
monitor.params = c('z','mu.p','u.b','v.b','mu.u.b','tau.u.b','mu.v.b','tau.v.b','LV','lv.coef')

#Specify the initial values  #all data in data.list can be assumed to be available in this function
initsfunction = function(chain) {
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
  
  .RNG.seed <- c(1, 2,3,4)[chain] # run jags likes to have this and the following argument defined in the initlalization functions.
  .RNG.name <- c(rep(c("base::Super-Duper", "base::Wichmann-Hill"),2))[chain]
  
  list(
    u.b= NULL,  #initial values guestimated from u.b.proto are erroring! "u[14,1]: Node inconsistent with parents"
    v.b= v.b,
    u=(y.occ.mock>0)-runif(1,0.1,0.8),  #this looks strange -> step(u) is an indicator of whether occupied or not
    #mu.a = matrix(rbinom((n)*J, size=1, prob=1),
    #              nrow=J, ncol=(n)),
    #lv.coef=matrix(runif(nlv*n,-1,1),n,nlv)*lv.coef,
    LV= matrix(rnorm(nlv*J),J,nlv),
    #z = matrix(rbinom((n)*J, size=1, prob=1),
    #           nrow=J, ncol=(n))
    .RNG.seed = .RNG.seed,
    .RNG.name = .RNG.name
  )
}

# set up initial values, potentially save them
inits <- list(inits1=initsfunction(1),inits2=initsfunction(2),inits3=initsfunction(3),inits4=initsfunction(4))


#### RUNNING JAGS ######
#run a quick test of the model in JAGS with R2jags
library(R2jags)
testfit.time <- system.time(testfit <- R2jags::jags(data = data.list,
                        inits = list(inits$inits1),
                        parameters.to.save = monitor.params,
                        model.file = modelFile,
                        n.chains = 1,
                        n.iter = 5,
                        n.burnin = 3,
                        n.thin = 1))
testfit$time <- testfit.time

library(runjags)
fit.runjags <- run.jags(modelFile,
                        n.chains = 2,
                        data = data.list,
                        inits = inits[1:2],
                        method = 'parallel',
                        monitor = monitor.params,
                        adapt = 4000,
                        burnin = 20000,
                        sample = 1000,
                        thin = 50) 

# mcmctime <- system.time(fit <- jags.parallel(occ.data, inits = occ.inits, occ.params, modelFile,
#                                              # n.chains=1, n.iter=3, n.burnin=1, n.thin=1))
#                                              n.chains=3, n.iter=1000, n.burnin=0, n.thin=1,
#                                              n.cluster = 1))
# saveRDS(fit, "./tmpdata/7_1_mcmcchain_20200327.rds") #282 megabytes
#took 6 hours on my laptop overnight


