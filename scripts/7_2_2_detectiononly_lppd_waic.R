source("./functions/calcpredictions.R")
source("./run_detectionaccuracy.R")
fit <- readRDS("./tmpdata/deto_wind.rds")
fit$data <- as.list.format(fit$data)
system.time(wind_lppd <- lppd.newdata(fit,
                            Xocc = inputdata$holdoutdata$occ_covariates,
                            yXobs = inputdata$holdoutdata$plotsmerged_detection,
                            ModelSite = "ModelSiteID"))


filenames <- list(
wind = "./tmpdata_deto_wind.rds",
time = "./tmpdata_deto_time.rds",
windtime = "./tmpdata_deto_windtime.rds",
windtemp = "./tmpdata_deto_windtemp.rds",
windtemp_time = "./tmpdata_deto_windtemp_time.rds",
windtimetemp_clouds = "./windtimetemp_clouds.rds",
windtimeclouds_temp = "./windtimeclouds_temp.rds",
windtimecloudstemp = "./windtimecloudstemp.rds")

cl <- makeCluster(15)
# lppds:
inputdata <- readRDS("./private/data/clean/7_2_1_input_data.rds")
Xocc = inputdata$holdoutdata$occ_covariates
yXobs = inputdata$holdoutdata$plotsmerged_detection
ModelSite = "ModelSiteID"

lppds <- lapply(filenames, function(x){
  fit <- readRDS(x)
  lppd <- lppd.newdata(fit,
               Xocc = Xocc,
               yXobs = yXobs,
               ModelSite = "ModelSiteID",
               cl = cl)
  return(lppd)
})
saveRDS(lppds, file = "./tmpdata/7_2_2_lppds.rds")


waics <- lapply(filenames, function(x){
  fit <- readRDS(x)
  likel.mat <- likelihoods.fit(fit,
               cl = cl)
  waic <- loo::waic(log(likel.mat))
  looest <- loo::loo(log(likel.mat))
  return(list(
    waic = waic,
    loo = looest
  ))
})
saveRDS(lppds, file = "./tmpdata/7_2_2_lppds.rds")



stopCluster(cl)

