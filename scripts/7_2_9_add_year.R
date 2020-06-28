devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_8_input_data.rds")

modelspecs <- list(
  interceptsonly = list(OccFmla = "~ 1",  #useful because we have different set of modelsites now (no 1998 visits)
                        ObsFmla = "~ 1"),
  year               = list(OccFmla = "~ 1 + SurveyYear",
                            ObsFmla = "~ 1"),
  twiwoody500m       = list(OccFmla = "~ 1 + TWI * woody500m", 
                            ObsFmla = "~ 1"),
  twiwoody500m_year       = list(OccFmla = "~ 1 + TWI * woody500m + SurveyYear", 
                            ObsFmla = "~ 1"),
  twiwoody500m_year_time       = list(OccFmla = "~ 1 + TWI * woody500m + SurveyYear", 
                                 ObsFmla = "~ 1 + MeanTime"),
  twiwoody500m_year_time_2lv   = list(OccFmla = "~ 1 + TWI * woody500m + SurveyYear", 
                                      ObsFmla = "~ 1 + MeanTime"),
  msnm       = list(OccFmla = "~ 1 + ms * NMdetected",
                    ObsFmla = "~ 1"),
  msnm_year       = list(OccFmla = "~ 1 + ms * NMdetected + SurveyYear",
                         ObsFmla = "~ 1"),
  msnm_year_time       = list(OccFmla = "~ 1 + ms * NMdetected + SurveyYear",
                         ObsFmla = "~ 1 + MeanTime"),
  msnm_year_time_2lv       = list(OccFmla = "~ 1 + ms * NMdetected + SurveyYear",
                              ObsFmla = "~ 1 + MeanTime")
)

modelspecs <- sapply(names(modelspecs), function(x) {
  modspec <- modelspecs[[x]]
  modspec$filename <- paste0("./tmpdata/7_2_9_addyear_", x,".rds")
  if (grepl("_2lv$", x)) {modspec$nlv <- 2} else {modspec$nlv = 0}
  return(modspec)},
  USE.NAMES = TRUE,
  simplify = FALSE)
saveRDS(modelspecs, "./tmpdata/7_2_9_modelspecs.rds")

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_8_input_data.rds")
modelspecs <- readRDS("./tmpdata/7_2_9_modelspecs.rds")
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
Xocc = indata$holdoutdata$Xocc
yXobs = indata$holdoutdata$yXobs
ModelSite = "ModelSiteID"

cl <- parallel::makeCluster(20)
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
saveRDS(lpds, file = "./tmpdata/7_2_9_lpds.rds")

waics <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  # Start the clock!
  ptm <- proc.time()
  
  likel.mat <- likelihoods.fit(fit,
                               cl = cl)
  waicmsgs <- capture.output(waic <- loo::waic(log(likel.mat)))
  loomsgs <- capture.output(looest <- loo::loo(log(likel.mat), cores = length(cl)))
  
  # Stop the clock
  timetaken <- proc.time() - ptm
  
  out <- list(
    waic = waic,
    loo = looest,
    timetaken = timetaken,
    waicmsgs = waicmsgs,
    loomsgs = loomsgs
  )
  save(out, file = paste0("./tmpdata/WAICS/", basename(x)))
  
  return(out)
})
saveRDS(waics, file = "./tmpdata/7_2_9_waics.rds")
parallel::stopCluster(cl)

loo_warnings <- warnings()
saveRDS(loo_warnings, file = "./tmpdata/WAICS/7_2_9_warnings.rds")
