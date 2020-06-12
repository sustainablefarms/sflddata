# after assessment, more smaller models
library(dplyr)
indata <- readRDS("./private/data/clean/7_2_1_input_data.rds")
library(sustfarmld)

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

deto_temp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ MeanTemp",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/deto_temp.rds"
)


deto_timetemp <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ MeanTemp * MeanTime",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/deto_timetemp.rds"
)

deto_clouds <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ MeanClouds",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/deto_clouds.rds"
)

deto_clouds_time_temp_wind <- run.detectionoccupany(
  Xocc = inputdata$occ_covariates,
  yXobs = inputdata$plotsmerged_detection,
  species = inputdata$detection_data_specieslist,
  ModelSite = "ModelSiteID",
  OccFmla = "~ 1",
  ObsFmla = "~ MeanClouds + MeanWind + MeanTime + MeanTemp",
  nlv = 2,
  MCMCparams = list(n.chains = 1, adapt = 1000, burnin = 10000, sample = 500, thin = 40),
  filename = "./tmpdata/deto_clouds_time_temp_wind.rds"
)