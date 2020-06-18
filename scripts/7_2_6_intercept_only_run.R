# occupancy models (ground-based observations) combined with detection models

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")

intercepts_only_nolv <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = function(chain, indata){return(NULL)},
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/intercepts_only_nolv.rds"
)
