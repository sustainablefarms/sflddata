# Running all detection models (separated manually into different scripts for now)

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
source("./functions/run_detectionaccuracy.R")

deto_wind <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./runjags_deto_wind"),
  filename = "./tmpdata_deto_wind.rds"
)

deto_wind_June4 <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40,
                    keep.jags.files = "./runjags_deto_wind_June4"),
  filename = "./tmpdata/deto_wind_June4.rds"
)

deto_time <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanTime",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./runjags_deto_time"),
  filename = "./tmpdata_deto_time.rds"
)


deto_windtime <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTime",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./runjags_deto_windtime"),
  filename = "./tmpdata_deto_windtime.rds"
)

deto_windtemp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTemp",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./runjags_deto_windtemp"),
  filename = "./tmpdata_deto_windtemp.rds"
)

deto_windtemp_time <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTemp + MeanTime",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./runjags_deto_windtemp_time"),
  filename = "./tmpdata_deto_windtemp_time.rds"
)

deto_windtimetemp_clouds <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTime * MeanTemp + MeanClouds",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./windtimetemp_clouds"),
  filename = "./windtimetemp_clouds.rds"
)

deto_windtimeclouds_temp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTime * MeanClouds + MeanTemp",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./windtimeclouds_temp"),
  filename = "./windtimeclouds_temp_June4.rds"
)
# ERRORED: terminate called after throwing an instance of 'jags::NodeError'
# what():  Node inconsistent with parents
# saved to ./windtimeclouds_temp_1

deto_windtimecloudstemp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ 1 + MeanWind * MeanTime * MeanClouds * MeanTemp",
  nlv = 2,
  MCMCparams = list(keep.jags.files = "./windtimecloudstemp"),
  filename = "./windtimecloudstema_June4.rds"
)
# Errored: terminate called after throwing an instance of 'jags::NodeError'
# what():  Node inconsistent with parents
# Warning messages:
#   1: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 2: glm.fit: fitted probabilities numerically 0 or 1 occurred 
# 3: glm.fit: fitted probabilities numerically 0 or 1 occurred 