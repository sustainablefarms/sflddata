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
#y          Matrix, 1 row per visit. Detection of species at a visit. The site of each visit given by ModelSite. Values are either 0 or 1.
#ModelSite   is a list of the (model) site observed for each visit. Note these are locations in space and TIME. (i.e same spatial location at different years is a different model site)
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
source("./scripts/7_2_1_groundobs_models_import_data.R")


# detection covariates of wind, survey time, only a constant as occupancy covariate
# from MW: "My instinct is to sum sub-shrubs and all the ground cover categories, 
# regardless of whether they are native or exotic; but perhaps excluding annual forms
# and grasses as those vary throughout the year. I’m not sure this will be critical 
# for birds but is probably worth testing."
Xocc <- model.matrix(as.formula("~ `% Native overstory cover` + `% Native midstory cover` * NMdetected +
I(`Exotic sub-shrub` + `Native sub-shrub` + 
   Cryptograms + `Native forbs/herbs/other` + `Organic litter` + `Exotic broadleaf/forb/other` +
   + `Coarse woody debris`
   - `Bare ground` - Rock )"),
                     data = occ_covariates) 
colnames(Xocc) <- c("(Intercept)", "NatOSCov", "NatMSCov", "NMdetected", "SumGroundCovers",
                       "NatMSCov:NMdetected")
Xocc[, -1] <- scale(Xocc[, -1]) #remove first column, which is intercept
Xobs <- model.matrix(as.formula("~ MeanWind + MeanTime + MeanClouds + MeanTemp + 1"), data = plotsmerged_detection)
Xobs[, -1] <- scale(Xobs[, -1])

### Latent variable multi-species co-occurence model
modelFile='./scripts/7_2_1_model_description.txt'

#Specify the data
nlv = 10 #Number of latent variables to use
n = length(detection_data_specieslist) #number of species
J <- nrow(Xocc)  #number of unique sites should also be max(occ_covariates$SiteID)
y <- as.matrix(plotsmerged_detection[, detection_data_specieslist])
ModelSite <- plotsmerged_detection$ModelSiteID
data.list = list(n=n, J=J, y=y,
                ModelSite = ModelSite, #a list of the site visited at each visit
                Vvisits = nrow(Xobs), #number of visits in total - not sure what this is for
                Xocc=Xocc,Xobs=Xobs,Vocc=ncol(Xocc),Vobs=ncol(Xobs),nlv=nlv)

#Specify the parameters to be monitored
monitor.params = c('u.b','v.b','mu.u.b','tau.u.b','mu.v.b','tau.v.b','lv.coef')
noread.monitor.params = c("LV")

### Initial conditions
## this is calculated just to get initial values for occupancy covariates
y.occ.mock <- plotsmerged_detection %>%
  group_by(ModelSiteID) %>%
  summarise_at(.vars = tidyselect::all_of(detection_data_specieslist), max) %>%
  dplyr::select(-ModelSiteID)

#Specify the initial values using a function
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
  # u.b.proto <- sapply(colnames(y),
  #               function(x) {unname(coef(glm(((y.occ.mock>0)*1)[, x] ~ Xocc[, -1],  
  #                                            family=binomial(link=probit))))},
  #               simplify = TRUE)
  # u.b <- t(u.b.proto)
  
  .RNG.seed <- c(1, 2, 3, 4, 5)[chain] # run jags likes to have this and the following argument defined in the initlalization functions.
  .RNG.name <- c(rep(c("base::Super-Duper", "base::Wichmann-Hill"),3))[chain]
  
  list(
    # u.b= NULL,  #initial values guestimated from u.b.proto are erroring! "u[14,1]: Node inconsistent with parents"
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
inits <- list(inits1=initsfunction(1), inits2=initsfunction(2))


#### RUNNING JAGS ######
library(runjags)
mcmctime <- system.time(fit.runjags <- run.jags(modelFile,
                        n.chains = 2,
                        data = data.list,
                        inits = inits,
                        method = 'parallel',
                        monitor = monitor.params,
                        noread.monitor = noread.monitor.params,
                        adapt = 4000,
                        burnin = 30000,
                        sample = 2000,
                        thin = 50,
                        keep.jags.files = TRUE,
                        tempdir = FALSE))
# writeLines(failed.jags('data')$data, "failedjags_data.txt") #useful command for debugging

# note that for simulation studies Tobler et al says they ran 3 chains drew 15000 samples, after a burnin of 10000 samples and thinning rate of 5.
# In the supplementary material it appears these parameters were used: n.chains=3, n.iter=20000, n.burnin=10000, n.thin=10. Experiment 7_1 suggested a higher thinning rate
fit.runjags$mcmctime <- mcmctime
saveRDS(fit.runjags, "./tmpdata/7_2_1_mcmcchain_20200516.rds") 

## try out runjag's cross-validation
kfoldres <- drop.k(fit.runjags, dropvars = y[1:30, 1:81], n.cores = 2)
saveRDS(fit.runjags, "./tmpdata/7_2_1_kfoldresult_20200516.rds") 

