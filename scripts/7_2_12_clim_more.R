devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_10_input_data.rds")

modelspecs <- list(
  someclimate_year = 
    list(OccFmla = "~ 1 + AnnMeanTemp + AnnPrec + AnnTempRange + PrecSeasonality +
        SurveyYear + PrecWarmQ", 
                            ObsFmla = "~ 1"),
  moreclimate_year = 
    list(OccFmla = "~ 1 + AnnMeanTemp + AnnPrec + AnnTempRange + PrecSeasonality + 
         SurveyYear + PrecWarmQ +
         MnTDryQ + PrecDryMonth", 
         ObsFmla = "~ 1")
)

# prep <- prep.designmatprocess(indata$insampledata$Xocc, modelspecs[[1]]$OccFmla)
# XoccDes <- apply.designmatprocess(prep, indata$insampledata$Xocc)

modelspecs <- sapply(names(modelspecs), function(x) {
  modspec <- modelspecs[[x]]
  modspec$filename <- paste0("./tmpdata/7_2_12_clim_", x,".rds")
  if (grepl("_2lv$", x)) {modspec$nlv <- 2} else {modspec$nlv = 0}
  return(modspec)},
  USE.NAMES = TRUE,
  simplify = FALSE)
saveRDS(modelspecs, "./tmpdata/7_2_12_modelspecs.rds")

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_10_input_data.rds")
modelspecs <- readRDS("./tmpdata/7_2_12_modelspecs.rds")
runfun <- function(x) {
  fit <- run.detectionoccupancy(
    Xocc = indata$insampledata$Xocc,
    yXobs = indata$insampledata$yXobs,
    species = indata$species,
    ModelSite = "ModelSiteID",
    OccFmla = x$OccFmla,
    ObsFmla = x$ObsFmla,
    nlv = x$nlv,
    initsfunction = function(chain, indata){return(NULL)},
    MCMCparams = list(n.chains = 2 - as.numeric(x$nlv > 0), adapt = 1000, burnin = 10000, sample = 500, thin = 40),
    filename = x$filename
  )
  return(fit)
}

fit <- runfun(modelspecs[[2]])

Sys.sleep(3600)

##### LPD and WAIC #####
### Compute holdout lpd and WAIC
filenames <- lapply(modelspecs, function(x) x$filename)
Xocc = indata$holdoutdata$Xocc
yXobs = indata$holdoutdata$yXobs
ModelSite = "ModelSiteID"

cl <- parallel::makeCluster(10)
lpds <- pbapply::pblapply(filenames, function(x){
  fit <- readRDS(x)
  fit$data <- as_list_format(fit$data)
  # Start the clock!
  ptm <- proc.time()
  
  lppd <- lppd.newdata(fit,
                       Xocc = Xocc,
                       yXobs = yXobs,
                       ModelSite = "ModelSiteID",
                       cl = cl)
  
  # Stop the clock
  timetaken <- proc.time() - ptm
  return(c(lppd, timetaken))
})
saveRDS(lpds, file = "./tmpdata/7_2_12_lpds.rds")

waics <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  # Start the clock!
  ptm <- proc.time()

  likel.mat <- likelihoods.fit(fit, chain = NULL,
                               cl = cl)
  chain_id <- lapply(1:length(fit$mcmc), function(x) rep(x, nrow(fit$mcmc[[x]])))
  chain_id <- as.integer(unlist(chain_id))
  waic <- loo::waic(log(likel.mat))
  r_eff <- loo::relative_eff(likel.mat, chain_id = chain_id, cores = length(cl))
  looest <- loo::loo(log(likel.mat), r_eff = r_eff, cores = length(cl))
  
  # Stop the clock
  timetaken <- proc.time() - ptm
  
  out <- list(
    waic = waic,
    loo = looest,
    timetaken = timetaken
  )
  save(out, file = paste0("./tmpdata/WAICS/", basename(x)))
  
  return(out)
})
saveRDS(waics, file = "./tmpdata/7_2_12_waics.rds")
parallel::stopCluster(cl)

loo_warnings <- warnings()
saveRDS(loo_warnings, file = "./tmpdata/WAICS/7_2_12_warnings.rds")
