devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")

grnd_pars <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1 + os + ms * NMdetected + gc",
  ObsFmla = "~ 1",
  nlv = 0,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/grnd_pars_nolv_properinits.rds"
)
