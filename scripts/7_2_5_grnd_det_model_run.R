# occupancy models (ground-based observations) combined with detection models

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")

grnd_msnm_time <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + ms * NMdetected",
  ObsFmla = "~ 1 + MeanTime",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_msnm_time_nolv.rds"
)

grnd_os_msnm_gc_time <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1 + MeanTime",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_msnm_gc_time_nolv.rds"
)

grnd_os_msnm_gc_time_wind <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1 + MeanTime + MeanWind",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_msnm_gc_time_wind_nolv.rds"
)

grnd_os_msnm_gc_timewind <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1 + MeanTime*MeanWind",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_msnm_gc_timewind_nolv.rds"
)

grnd_os_msnm_gc_timewind_temp_clouds <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1 + MeanTime*MeanWind + MeanTemp + MeanClouds",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_msnm_gc_timewind_temp_clouds_nolv.rds"
)

grnd_os_msnm_gc_2ndO <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + I(os^2) + ms * NMdetected + I(ms^2) * NMdetected + gc + I(gc^2)",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_msnm_gc_2ndO_nolv.rds"
)

grnd_os_msnm_gc_2ndO_timewind_temp_clouds <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + I(os^2) + ms * NMdetected + I(ms^2) * NMdetected + gc + I(gc^2)",
  ObsFmla = "~ 1 + MeanTime*MeanWind + MeanTemp + MeanClouds",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_msnm_gc_2ndO_timewind_temp_clouds_nolv.rds"
)