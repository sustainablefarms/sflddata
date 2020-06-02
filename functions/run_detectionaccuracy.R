


#' 
#' @param Xocc A dataframe of covariates related to occupancy. One row per ModelSite.
#' Must also include the ModelSiteVars columns to uniquely specify ModelSite.
#' @param yXobs A dataframe of species observations (1 or 0) and covariates related to observations. One row per visit.
#' Each column is either a covariate or a species.
#' Must also include the ModelSiteVars columns to uniquely specify ModelSite.
#' @param species A list of species names (corresponding to columns of yXobs) to model.
#' @param OccFmla A formula specifying the occupancy model in terms of the covariates in Xocc.
#' @param ObsFmla A formula specifying the model for detection in terms of the covariates in Xobs.
#' @param ModelSite A list of column names in y, Xocc and Xobs that uniquely specify the ModelSite. Can be simply a ModelSite index
#' @param nlv The number of latent variables
#' @param MCMCparams A named list containing values for n.chains, adapt, burnin, sample and thin (to pass to run.jags). 
#' Also include keep.jags.files to specify the directory that JAGS data will be saved.
#' @param filename If non-null the runjags object (with some extra information) is saved to filename as an RDS.
run.detectionoccupany <- function(Xocc, yXobs, species, ModelSite, OccFmla = "~ 1", ObsFmla = "~ 1", nlv = 2,
                                  MCMCparams = list(n.chains = 1, adapt = 2000, burnin = 25000, sample = 1000, thin = 30),
                                  filename = NULL){
  # check data inputs
  stopifnot(all(ModelSite %in% names(Xocc)))
  stopifnot(all(ModelSite %in% names(yXobs)))
  stopifnot(all(species %in% names(yXobs)))
  stopifnot(anyDuplicated(Xocc[, ModelSite]) == 0) #model site uniquely specified
  stopifnot(all(as.matrix(yXobs[, species]) %in% c(1, 0)))
  
  # create model site indexes
  # if (ModelSiteID %in% c(names(yXobs), Xocc)){warning("Overwriting ModelSiteID column in input data.")}
  ModelSiteMat <- cbind(1:nrow(Xocc), Xocc[, ModelSite])
  visitedModelSiteMat <- dplyr::right_join(ModelSiteMat, yXobs[, ModelSite], by = ModelSite, suffix = c("", ".in"))
  visitedModelSite <- visitedModelSiteMat[, 1]
  stopifnot(is.integer(visitedModelSite))
  stopifnot(all(visitedModelSite <= nrow(Xocc)))
  
  
  
  XoccProcess <- prep.designmatprocess(Xocc, OccFmla)
  XoccDesign <- apply.designmatprocess(XoccProcess, Xocc)
  XobsProcess <- prep.designmatprocess(yXobs, ObsFmla)
  XobsDesign <- apply.designmatprocess(XobsProcess, yXobs)
  stopifnot("(Intercept)" == colnames(XobsDesign)[[1]]) #check intercept in first column  - for the v.b.proto initialisation below
  stopifnot(ncol(XobsDesign) > 1) # check more than 1 column - for the v.b.proto initialisation below

  ### Latent variable multi-species co-occurence model
  modelFile='./scripts/7_2_1_model_description.txt'

  #Specify the data
  # nlv = nlv #Number of latent variables to use
  n = length(species) #number of species
  J <- nrow(XoccDesign)  #number of unique sites should also be max(occ_covariates$SiteID)
  y <- as.matrix(yXobs[, species])
  ModelSite <- visitedModelSite
  data.list = list(n=n, J=J, y=y,
                  ModelSite = ModelSite, #a list of the site visited at each visit
                  Vvisits = nrow(XobsDesign), #number of visits in total - not sure what this is for
                  Xocc=XoccDesign,Xobs=XobsDesign,Vocc=ncol(XoccDesign),Vobs=ncol(XobsDesign),nlv=nlv)

  #Specify the parameters to be monitored
  monitor.params = c('u.b','v.b','mu.u.b','tau.u.b','mu.v.b','tau.v.b','lv.coef', "LV")

  ### Initial conditions
  ## this is calculated just to get initial values for occupancy covariates
  y.occ.mock <- cbind(ModelSiteID = visitedModelSite, y) %>%
    as_tibble() %>%
    group_by(ModelSiteID) %>%
    summarise_all(max) %>%
    dplyr::select(-ModelSiteID)

  #Specify the initial values using a function
  initsfunction = function(chain) {
    lv.coef<-matrix(1,n,nlv)
    lv.coef[1:nlv,1:nlv]<-0
    for(l in 1:nlv-1){
      lv.coef[l,(l+1):nlv]<-NA
    }
    v.b.proto <- sapply(colnames(y),
                        function(x) {unname(coef(glm(((y>0)*1)[, x] ~ XobsDesign[, -1],  
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
  inits <- lapply(1:MCMCparams$n.chains, initsfunction)

#### RUNNING JAGS ######
  runjagsargs <- list(
    model = modelFile,
    n.chains = MCMCparams$n.chains,
    data = data.list,
    inits = inits,
    method = 'parallel',
    monitor = monitor.params,
    adapt = 2000,
    burnin = 25000,
    sample = 1000,
    thin = 30,
    keep.jags.files = TRUE,
    tempdir = FALSE
  )
  runjagsargs[names(MCMCparams)] <- MCMCparams
  library(runjags)
  mcmctime <- system.time(fit.runjags <- do.call(run.jags, args = runjagsargs))
  
  fit.runjags$mcmctime <- mcmctime
  # note that for simulation studies Tobler et al says they ran 3 chains drew 15000 samples, after a burnin of 10000 samples and thinning rate of 5.
  # In the supplementary material it appears these parameters were used: n.chains=3, n.iter=20000, n.burnin=10000, n.thin=10. Experiment 7_1 suggested a higher thinning rate

  # add summary of parameter distributions
  fit.runjags <- add.summary(fit.runjags)

  # attach data preparation methods
  fit.runjags$XoccProcess <- XoccProcess
  fit.runjags$XobsProcess <- XobsProcess
  saveRDS(fit.runjags, filename) 
  invisible(fit.runjags)
}



prep.designmatprocess <- function(indata, fmla){
  designmat1 <- model.matrix(as.formula(fmla), indata)
  means <- colMeans(designmat1)
  sds <- ((nrow(designmat1) - 1) / nrow(designmat1)) * apply(designmat1, 2, sd)
  center <- means
  scale <- sds
  center[sds * means < 1E-8] <- means[sds * means < 1E-8] - 1 #centering of constant columns to 1
  scale[sds * means < 1E-8] <- 1 #no scaling of constant columns
  preprocessobj <- list(fmla = fmla, center = center, scale = scale)
  return(preprocessobj)
}
apply.designmatprocess <- function(designmatprocess, indata){
  designmat1 <- model.matrix(as.formula(designmatprocess$fmla), indata)
  designmat <- scale(designmat1, center = designmatprocess$center, scale = designmatprocess$scale)
  return(designmat)
}

#### Examples #####
#' 
#' @examples 
inputdata <- readRDS("./private/data/clean/7_2_1_input_data.rds")
fitjags <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ MeanWind * MeanTime + MeanClouds",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 0, burnin = 0, sample = 1, thin = 1),
  filename = "./test1.rds"
)
Xocc <- inputdata$occ_covariates
ModelSite <- c("ModelSiteID")
yXobs <- inputdata$plotsmerged_detection
species <- inputdata$detection_data_specieslist
OccFmla = "~ 1"
ObsFmla = "~  MeanWind * MeanTime"
