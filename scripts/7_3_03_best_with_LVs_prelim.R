devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_10_input_data.rds")

modelspecs <- list(
  someclimate_year_woody500m_msnm_det1stO_1lv = 
    list(OccFmla = "~ 1 + AnnMeanTemp + AnnPrec + AnnTempRange + PrecSeasonality +
        SurveyYear + PrecWarmQ + woody500m + ms * NMdetected", 
         ObsFmla = "~ 1 + MeanTime + MeanTemp + MeanWind + MeanClouds",
         nlv = 1),
  someclimate_year_woody500m_msnm_det1stO_2lv = 
    list(OccFmla = "~ 1 + AnnMeanTemp + AnnPrec + AnnTempRange + PrecSeasonality +
        SurveyYear + PrecWarmQ + woody500m + ms * NMdetected", 
         ObsFmla = "~ 1 + MeanTime + MeanTemp + MeanWind + MeanClouds",
         nlv = 2),
  someclimate_year_woody500m_msnm_det1stO_3lv = 
    list(OccFmla = "~ 1 + AnnMeanTemp + AnnPrec + AnnTempRange + PrecSeasonality +
        SurveyYear + PrecWarmQ + woody500m + ms * NMdetected", 
         ObsFmla = "~ 1 + MeanTime + MeanTemp + MeanWind + MeanClouds",
         nlv = 3),
  someclimate_year_woody500m_msnm_det1stO_4lv = 
    list(OccFmla = "~ 1 + AnnMeanTemp + AnnPrec + AnnTempRange + PrecSeasonality +
        SurveyYear + PrecWarmQ + woody500m + ms * NMdetected", 
         ObsFmla = "~ 1 + MeanTime + MeanTemp + MeanWind + MeanClouds",
         nlv = 4)
)

modelspecs <- sapply(names(modelspecs), function(x) {
  modspec <- modelspecs[[x]]
  modspec$filename <- paste0("./tmpdata/7_3_02_clim_", x,".rds")
  return(modspec)},
  USE.NAMES = TRUE,
  simplify = FALSE)
saveRDS(modelspecs, "./tmpdata/7_3_03_modelspecs.rds")

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_10_input_data.rds")
modelspecs <- readRDS("./tmpdata/7_3_03_modelspecs.rds")
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

fit <- runfun(modelspecs[[]])

##### LPD and WAIC #####
### Compute holdout lpd and WAIC
filenames <- lapply(modelspecs, function(x) x$filename)
a <- vapply(filenames, file.exists, FUN.VALUE = FALSE)
stopifnot(all(a))
# filenames <- filenames[a]
Xocc = indata$holdoutdata$Xocc
yXobs = indata$holdoutdata$yXobs
ModelSite = "ModelSiteID"

cl <- parallel::makeCluster(30)
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
saveRDS(lpds, file = "./tmpdata/7_3_03_lpds.rds")

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
saveRDS(waics, file = "./tmpdata/7_3_03_waics.rds")
parallel::stopCluster(cl)

loo_warnings <- warnings()
saveRDS(loo_warnings, file = "./tmpdata/WAICS/7_3_03_warnings.rds")
