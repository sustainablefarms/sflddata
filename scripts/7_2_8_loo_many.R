# Script for calculating the LOO-PSIS estimating using r_eff (all previous version computed it without r_eff)

library(sustfarmld)
library(tidyr)
library(dplyr)

modelspecs_7_2_7 <- list(
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

modelspecs_7_2_7 <- sapply(names(modelspecs_7_2_7), function(x) {
  modspec <- modelspecs_7_2_7[[x]]
  modspec$filename <- paste0("./tmpdata/twospecies_", x,".rds")
  return(modspec)},
  USE.NAMES = TRUE,
  simplify = FALSE)

names(modelspecs_7_2_7) <- paste0("sp2_", names(modelspecs_7_2_7))

modelspecs_7_2_8 <- readRDS("./tmpdata/7_2_8_modelspecs.rds")

filenames <- c(lapply(modelspecs_7_2_7, function(x) x$filename),
  lapply(modelspecs_7_2_8, function(x) x$filename), 
  list(
  os = "./tmpdata/grnd_os_nolv.rds",
  ms = "./tmpdata/grnd_os_ms_nolv.rds",
  os_gc = "./tmpdata/grnd_os_gc_nolv.rds",
  nm = "./tmpdata/grnd_nm_nolv.rds",
  msnm = "./tmpdata/grnd_msnm_nolv.rds",
  pars = "./tmpdata/grnd_pars_nolv.rds"
), list(
  os_msnm_gc_2ndO = "./tmpdata/grnd_os_msnm_gc_2ndO_nolv.rds",
  msnm_time = "./tmpdata/grnd_msnm_time_nolv.rds",
  os_msnm_gc_time = "./tmpdata/grnd_os_msnm_gc_time_nolv.rds",
  os_msnm_gc_time_wind = "./tmpdata/grnd_os_msnm_gc_time_wind_nolv.rds",
  os_msnm_gc_timewind = "./tmpdata/grnd_os_msnm_gc_timewind_nolv.rds",
  os_msnm_gc_timewind_temp_clouds = "./tmpdata/grnd_os_msnm_gc_timewind_temp_clouds_nolv.rds",
  os_msnm_gc_2ndO_timewind_temp_clouds = "./tmpdata/grnd_os_msnm_gc_2ndO_timewind_temp_clouds_nolv.rds"
),
  intercepts_only = "./tmpdata/intercepts_only_nolv.rds"
)


# test loading models
a <- vapply(filenames, file.exists, FUN.VALUE = FALSE)
stopifnot(all(a))

cl <- parallel::makeCluster(20)
parallel::clusterEvalQ(cl, library(sustfarmld))
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
  save(out, file = paste0("./tmpdata/WAICS2/", basename(x)))
  
  return(out)
})