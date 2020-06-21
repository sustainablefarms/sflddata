devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_8_input_data.rds")

# covariates: "woody500m", "TWI", "GPPmean", "GPPdiff"

modelspecs <- list(
  interceptsonly = list(OccFmla = "~ 1",  #useful because we have different set of modelsites now (no 1998 visits)
                        ObsFmla = "~ 1"),
  twi                = list(OccFmla = "~ 1 + TWI",
                            ObsFmla = "~ 1"),
  woody500m          = list(OccFmla = "~ 1 + woody500m",
                            ObsFmla = "~ 1"),
  gppmean            = list(OccFmla = "~ 1 + GPPmean",
                            ObsFmla = "~ 1"),  
  gppdiff            = list(OccFmla = "~ 1 + GPPdiff",
                            ObsFmla = "~ 1"),  
  twigppmean         = list(OccFmla = "~ 1 + TWI * GPPmean", #two time constant things somewhat related to terrain
                            ObsFmla = "~ 1"),
  twiwoody500m       = list(OccFmla = "~ 1 + TWI * woody500m", #the non-gpp things
                            ObsFmla = "~ 1"),
  twiwoody500mgppdiff = list(OccFmla = "~ 1 + TWI * woody500m * GPPdiff", 
                            ObsFmla = "~ 1"),
  twiwoody500mgppdiff_gppmean = list(OccFmla = "~ 1 + TWI * woody500m * GPPdiff + GPPmean", 
                            ObsFmla = "~ 1"),
  twiwoody500mgppdiffgppmean = list(OccFmla = "~ 1 + TWI * woody500m * GPPdiff * GPPmean", # full options
                            ObsFmla = "~ 1")
)

modelspecs <- sapply(names(modelspecs), function(x) {
  modspec <- modelspecs[[x]]
  modspec$filename <- paste0("./tmpdata/rsonly_", x,".rds")
  return(modspec)},
  USE.NAMES = TRUE,
  simplify = FALSE)
saveRDS("./tmpdata/7_2_8_modelspecs.rds")

devtools::load_all()
indata <- readRDS("./private/data/clean/7_2_8_input_data.rds")
modelspecs <- readRDS("./tmpdata/7_2_8_modelspecs.rds")
runfun <- function(x) {
  fit <- run.detectionoccupancy(
    Xocc = indata$insampledata$Xocc,
    yXobs = indata$insampledata$yXobs,
    species = in$data$species,
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

runfun(modelspecs[[1]])