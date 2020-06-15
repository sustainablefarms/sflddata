# occupancy models for ground-based analysis

indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")
devtools::load_all()

nlv = 0
list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40)

grnd_os <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + `% Native overstory cover`",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_nolv.rds"
)


grnd_os_ms <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + `% Native overstory cover` + `% Native midstory cover`",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_ms_nolv.rds"
)

grnd_os_grnd <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + `% Native overstory cover` + I(`Exotic sub-shrub` + `Native sub-shrub` + 
   Cryptograms + `Native forbs/herbs/other` + `Organic litter` + `Exotic broadleaf/forb/other` +
   + `Coarse woody debris` )",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_os_grnd_nolv.rds"
)

grnd_nm <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + NMdetected",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_nm_nolv.rds"
)

grnd_msnm <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + `% Native midstory cover` * NMdetected",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_msnm_nolv.rds"
)

grnd_pars <- run.detectionoccupany(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + `% Native overstory cover` + `% Native midstory cover` * NMdetected +
I(`Exotic sub-shrub` + `Native sub-shrub` + 
   Cryptograms + `Native forbs/herbs/other` + `Organic litter` + `Exotic broadleaf/forb/other` +
   + `Coarse woody debris` )",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_pars_nolv.rds"
)