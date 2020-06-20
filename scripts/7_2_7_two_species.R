
devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")
sort(colMeans(indata$insampledata$yXobs[, indata$species]))

species <- c("Galah", "Willie Wagtail") #all seen in about 0.3 of visits

modelspecs <- list(
  interceptsonly = list(OccFmla = "~ 1",
                        ObsFmla = "~ 1"),
  ms =  list(OccFmla = "~ 1 + ms",
             ObsFmla = "~ 1"),
  os_ms =  list(OccFmla = "~ 1 + os + ms",
                ObsFmla = "~ 1"),
  os_ms_nm       = list(OccFmla = "~ 1 + os + ms + NMdetected",
                        ObsFmla = "~ 1 "),
  os_ms_nm_gc    = list(OccFmla = "~ 1 + os + ms + NMdetected + gc",
                        ObsFmla = "~ 1 "),
  os_msnm_gc     = list(OccFmla = "~ 1 + os + ms * NMdetected + gc",
                        ObsFmla = "~ 1 "),
  msnm_time      = list(OccFmla = "~ 1 + ms * NMdetected ",
                        ObsFmla = "~ 1 + MeanTime"),
  msnm_time_temp = list(OccFmla = "~ 1 + ms * NMdetected",
                        ObsFmla = "~ 1 + MeanTime + MeanTemp"),
  msnm_timetemp  = list(OccFmla = "~ 1 + ms * NMdetected",
                        ObsFmla = "~ 1 + MeanTime * MeanTemp"))

modelspecs <- sapply(names(modelspecs), function(x) {
  modspec <- modelspecs[[x]]
  modspec$filename <- paste0("./tmpdata/twospecies_", x,".rds")
  return(modspec)},
  USE.NAMES = TRUE,
  simplify = FALSE)

cl <- parallel::makeCluster(length(modelspecs))
parallel::clusterEvalQ(cl, devtools::load_all())
parallel::clusterExport(cl, c("indata", "species"))
fittedmods <- pbapply::pblapply( 
                        modelspecs,
                        function(x) {
                          fit <- run.detectionoccupancy(
                            Xocc = indata$insampledata$Xocc,
                            yXobs = indata$insampledata$yXobs,
                            species = species,
                            ModelSite = "ModelSiteID",
                            OccFmla = x$OccFmla,
                            ObsFmla = x$ObsFmla,
                            nlv = 0,
                            initsfunction = function(chain, indata){return(NULL)},
                            MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
                            filename = x$filename
                          )
                          return(fit)
                        }
                        )
parallel::stopCluster(cl)

### Compute holdout lpd and WAIC
filenames <- lapply(modelspecs, function(x) x$filename)
Xocc = inputdata$holdoutdata$Xocc
yXobs = inputdata$holdoutdata$yXobs
ModelSite = "ModelSiteID"

cl <- parallel::makeCluster(length(modelspecs))
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
saveRDS(lpds, file = "./tmpdata/7_2_7_lpds.rds")

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
saveRDS(waics, file = "./tmpdata/7_2_7_waics.rds")
parallel::stopCluster(cl)

loo_warnings <- warnings()
saveRDS(loo_warnings, file = "./tmpdata/WAICS/7_2_7_warnings.rds")