library(dplyr)
indata <- readRDS("./private/data/clean/7_2_1_input_data.rds")

# fix two outlying start times:
sitemeans <- indata$plotsmerged_detection %>%
  dplyr::filter(MeanTime > 300) %>%
  group_by(SurveySiteId) %>%
  summarise(MeanVisitTime = mean(MeanTime)) %>%
  filter(SurveySiteId %in% c(1682, 1031)) %>%
  tibble::deframe()
indata$plotsmerged_detection[indata$plotsmerged_detection$ModelSiteID==1819 &
                               indata$plotsmerged_detection$RepeatNumber==1 & 
                               indata$plotsmerged_detection$SurveyYear==2016 & 
                               indata$plotsmerged_detection$SurveySiteId==1682, "MeanTime"] <- sitemeans["1682"]
indata$plotsmerged_detection[indata$plotsmerged_detection$ModelSiteID==1295 &
                               indata$plotsmerged_detection$RepeatNumber==2 & 
                               indata$plotsmerged_detection$SurveyYear==2011 & 
                               indata$plotsmerged_detection$SurveySiteId==1031, "MeanTime"] <- sitemeans["1031"]
saveRDS(indata, file = "./tmpdata/7_2_2_input_data.rds")

inputdata <- readRDS("./tmpdata/7_2_2_input_data.rds")
source("./R/run_detectionoccupancy.R")

deto_windtimeclouds_temp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTime * MeanClouds + MeanTemp",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 0, burnin = 0, sample = 1, thin = 1,
                    keep.jags.files = "./windtimeclouds_temp"),
  filename = "./windtimeclouds_temp.rds"
)
deto_windtimeclouds_temp$mcmc[[1]] %>%
  as_tibble() %>%
  select(matches("^u.b\\[")) %>%
  unlist() %>%
  hist()

deto_windtimecloudstemp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTime * MeanClouds * MeanTemp",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 0, burnin = 0, sample = 1, thin = 1,
                    keep.jags.files = "./windtimecloudstemp"),
  filename = "./windtimecloudstemp.rds"
)
failed.jags("output")
inits <- list.format(failed.jags("inits")[[1]])
inits$v.b[64, ]
