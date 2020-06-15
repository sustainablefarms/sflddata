# occupancy models for ground-based analysis

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")

grnd_os <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_nolv.rds"
)


grnd_os_ms <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_ms_nolv.rds"
)

grnd_os_gc <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + gc",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_gc_nolv.rds"
)

grnd_nm <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + NMdetected",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_nm_nolv.rds"
)

grnd_msnm <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + ms * NMdetected",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_msnm_nolv.rds"
)

grnd_pars <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_pars_nolv.rds"
)
