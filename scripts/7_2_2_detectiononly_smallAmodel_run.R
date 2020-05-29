########################################################################################################
## Script to run a latent variable multi-species co-occurrence model with imperfect detection.
## Supplemental material to: 
## Tobler et al. 2019 Joint species distribution models with species correlations 
## and imperfect detection, Ecology.
########################################################################################################

########################################################################################################
## Script to run a latent variable multi-species co-occurrence model with imperfect detection.
## Modified from 
## Tobler et al. 2019 Joint species distribution models with species correlations 
## and imperfect detection, Ecology.
########################################################################################################

### Guide to inputs names and dimensions  ####
#n.env.var  number of random environmental variables for occupancy
#J          number of 'sites' to fit occupancy and detection (is separated by year/season)
#             (fyi: j is used for the ModelSite index in the model description)
#y          Matrix, 1 row per visit. Detection of species at a visit. The site of each visit given by ModelSite. Values are either 0 or 1.
#ModelSite   is a list of the (model) site observed for each visit. Note these are locations in space and TIME. (i.e same spatial location at different years is a different model site)
#nlv        number of latent variables
#n          number of species
#Xocc       matrix of covariates to predict occupancy (include a column of 1s for intercept). 1 row per site.
#           (Rows ordered according to site index: must be compatible with ObservedSite.)
#Xobs       matrix of covariates to predict detection (include a column of 1s for intercept). 1 row per visit.
#Vvisits    number of visits in total
#Vobs       number of detection-related covariates
#Vocc       number of occupancy-related covariates

### within the model specification (which is primarly in 7_2_1_model_description: ####
# mu.u.b is a list of the *mean* of occupancy covariate (random) effects (one for each covariate) [has a Gaussian prior]
# tau.u.b is a list of the standard deviation of the occupany covariate effects
# sigma.u.b is a used to define tau.u.b (and has a uniform prior bounded by 10)
# u.b is an array giving the occupancy covariate coefficients. Each row corresponds to a species, each column is an occupancy covariate. 
# u.b is distributed as a Gaussian distribution with mean mu.u.b and standard deviation tau.u.b.
## In Tobler's paper 'species-level' effects are treated as random effects. To improve estimates of rare species and convergence.
## u.b combined with occupancy covariates and latent variables to give the mean of the occupancy random variable.


# mu.v.b is a list of the *mean* of detection covariate effects (one for each covariate)
# tau.v.b is a list of the *standard deviation* of the detection covariate effects
# sigma.v.b is a used to define tau.v.b
# v.b is an array. Each row corresponds to a species, each column to a detection covariate. Distributional params given by mu.v.b and tau.v.b.

# LV is a array for the value of latent variables. Each row corresponds to a site. Each column corresponds to a latent variable.
# lv.coef is a matrix of coefficients for the latent variables (loadings). 
#   Priors for lv.coef have restrictions to create identifiability in the sign of latent variables. 
#   Each row corresponds to a species. Each column is a latent variable.

# 

# library(MCMCpack)
# library(mclust)
# library(corrplot)
library(runjags)
library(dplyr)

# get data 
inputdata <- readRDS("./private/data/clean/7_2_1_input_data.rds")
  
# from MW: "My instinct is to sum sub-shrubs and all the ground cover categories, 
# regardless of whether they are native or exotic; but perhaps excluding annual forms
# and grasses as those vary throughout the year. I’m not sure this will be critical 
# for birds but is probably worth testing."
Xocc <- model.matrix(as.formula("~ 1"),
                     data = inputdata$occ_covariates) 
colnames(Xocc) <- c("(Intercept)")
Xocc.center <- NULL
Xocc.scale <- NULL

Xobs <- model.matrix(as.formula("~  MeanWind * MeanTime - 1"), data = inputdata$plotsmerged_detection)
Xobs <- scale(Xobs)
Xobs.center <- attr(Xobs, "scaled:center")
Xobs.scale <- attr(Xobs, "scaled:scale")
Xobs <- cbind("(Intercept)" = 1, scale(Xobs[, -1]))

### Latent variable multi-species co-occurence model
modelFile='./scripts/7_2_1_model_description.txt'

#Specify the data
nlv = 2 #Number of latent variables to use
n = length(inputdata$detection_data_specieslist) #number of species
J <- nrow(Xocc)  #number of unique sites should also be max(occ_covariates$SiteID)
y <- as.matrix(inputdata$plotsmerged_detection[, inputdata$detection_data_specieslist])
ModelSite <- inputdata$plotsmerged_detection$ModelSiteID
data.list = list(n=n, J=J, y=y,
                ModelSite = ModelSite, #a list of the site visited at each visit
                Vvisits = nrow(Xobs), #number of visits in total - not sure what this is for
                Xocc=Xocc,Xobs=Xobs,Vocc=ncol(Xocc),Vobs=ncol(Xobs),nlv=nlv)

#Specify the parameters to be monitored
monitor.params = c('u.b','v.b','mu.u.b','tau.u.b','mu.v.b','tau.v.b','lv.coef')
noread.monitor.params = c("LV")

### Initial conditions
## this is calculated just to get initial values for occupancy covariates
y.occ.mock <- inputdata$plotsmerged_detection %>%
  group_by(ModelSiteID) %>%
  summarise_at(.vars = tidyselect::all_of(inputdata$detection_data_specieslist), max) %>%
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

# set up initial values
n.chains <- 1
inits <- lapply(1:n.chains, initsfunction)

#### RUNNING JAGS ######
library(runjags)
mcmctime <- system.time(fit.runjags <- run.jags(modelFile,
                        n.chains = n.chains,
                        data = data.list,
                        inits = inits,
                        method = 'parallel',
                        monitor = monitor.params,
                        noread.monitor = noread.monitor.params,
                        adapt = 2000,
                        burnin = 25000,
                        sample = 1000,
                        thin = 30,
                        keep.jags.files = TRUE,
                        tempdir = FALSE))
fit.runjags$mcmctime <- mcmctime
# writeLines(failed.jags('data')$data, "failedjags_data.txt") #useful command for debugging

# note that for simulation studies Tobler et al says they ran 3 chains drew 15000 samples, after a burnin of 10000 samples and thinning rate of 5.
# In the supplementary material it appears these parameters were used: n.chains=3, n.iter=20000, n.burnin=10000, n.thin=10. Experiment 7_1 suggested a higher thinning rate

# add summary of parameter distributions
fit.runjags <- add.summary(fit.runjags)

# attached scale params
fit.runjags$Xobs.center <- Xobs.center
fit.runjags$Xobs.scale <- Xobs.scale
fit.runjags$Xocc.center <- Xocc.center
fit.runjags$Xocc.scale <- Xocc.scale

saveRDS(fit.runjags, "./tmpdata/7_2_2_detectiononly_smallAmodel_run_20200529.rds") 



