# occupancy models (ground-based observations) combined with detection models

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_4_input_data.rds")

initsfunction <- function(chain, indata){
  prepinits <- rep(c(FALSE, TRUE, TRUE, FALSE), 3)
  if (prepinits){inits <- defaultinitsfunction(chain, indata)}
  else {inits <- NULL}
  return(inits)
}

intercepts_only_nolv <- run.detectionoccupancy(
  Xocc = indata$insampledata$Xocc,
  yXobs = indata$insampledata$y,
  species = indata$species,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1",
  nlv = 0,
  initsfunction = initsfunction,
  MCMCparams = list(n.chains = 2, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/intercepts_only_nolv.rds"
)
