
devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")
sort(colMeans(indata$insampledata$yXobs[, indata$species]))

species <- c("Galah", "Willie Wagtail") #all seen in about 0.3 of visits

interceptsonly <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_intercepts.rds"
)


ms <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + ms",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_ms.rds"
)

os_ms <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms ",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_os_ms.rds"
)

os_ms_nm <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms + NMdetected",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_os_ms_nm.rds"
)

os_ms_nm_gc <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms + NMdetected + gc",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_os_ms_nm_gc.rds"
)

os_msnm_gc <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_os_msnm_gc.rds"
)


msnm_time <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + ms * NMdetected",
  ObsFmla = "~ 1 + time",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_msnm_time.rds"
)

msnm_time_temp <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + ms * NMdetected",
  ObsFmla = "~ 1 + time + temp",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_msnm_time_temp.rds"
)

msnm_timetemp <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + ms * NMdetected",
  ObsFmla = "~ 1 + time * temp",
  nlv = 0,
  initsfunction = function(x, y){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/twospecies_msnm_timetemp.rds"
)


