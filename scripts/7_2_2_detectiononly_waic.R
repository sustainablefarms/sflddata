# 7_2_2_detectiononly_waics.R

filenames <- list(
  wind = "./tmpdata_deto_wind.rds",
  wind_June4 =  "./tmpdata/deto_wind_June4.rds",
  time = "./tmpdata_deto_time.rds",
  windtime = "./tmpdata_deto_windtime.rds",
  windtemp = "./tmpdata_deto_windtemp.rds",
  windtemp_time = "./tmpdata_deto_windtemp_time.rds",
  windtimetemp_clouds = "./windtimetemp_clouds.rds",
  windtimeclouds_temp = "./windtimeclouds_temp_June4.rds",
  windtimecloudstemp = "./windtimecloudstema_June4.rds",
  clods_time_temp_wind = "./tmpdata/deto_clouds_time_temp_wind.rds",
  timetemp = "./tmpdata/deto_timetemp.rds",
  interactions_2nd_nolv = "./tmpdata/deto_interactions_2nd.rds",
  clouds = "./tmpdata/deto_clouds.rds",
  temp = "./tmpdata/deto_temp.rds"
  )

# test loading models
a <- vapply(filenames, file.exists, FUN.VALUE = FALSE)
stopifnot(all(a))

devtools::load_all()
library(dplyr); library(tibble); library(tidyr);
cl <- parallel::makeCluster(20)
waics <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  fit$data <- as.list.format(fit$data)
  colnames(fit$data$y) <- fit$species
  colnames(fit$data$Xocc) <- names(fit$XoccProcess$center)
  colnames(fit$data$Xobs) <- names(fit$XobsProcess$center)
  # Start the clock!
  ptm <- proc.time()
  
  likel.mat <- likelihoods.fit(fit,
                               cl = cl)
  waic <- loo::waic(log(likel.mat))
  looest <- loo::loo(log(likel.mat), cores = length(cl))
  
  # Stop the clock
  timetaken <- proc.time() - ptm
  
  out <- list(
    waic = waic,
    loo = looest,
    timetaken = timetaken
  )
  save(out, file = paste0("./tmpdata/WAICS/", x))
  
  return(out)
})
saveRDS(waics, file = "./tmpdata/7_2_2_waics.rds")
parallel::stopCluster(cl)

