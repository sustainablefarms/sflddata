source("./functions/calcpredictions.R")
source("./run_detectionaccuracy.R")


filenames <- list(
wind = "./tmpdata_deto_wind.rds",
time = "./tmpdata_deto_time.rds",
windtime = "./tmpdata_deto_windtime.rds",
windtemp = "./tmpdata_deto_windtemp.rds",
windtemp_time = "./tmpdata_deto_windtemp_time.rds")
# windtimetemp_clouds = "./windtimetemp_clouds.rds",
# windtimeclouds_temp = "./windtimeclouds_temp.rds",
# windtimecloudstemp = "./windtimecloudstemp.rds")

source("./functions/likelihood.R")
source("./functions/calcpredictions.R")
source("./functions/run_detectionaccuracy.R")
cl <- parallel::makeCluster(15)
parallel::clusterEvalQ(cl = cl,  source("./functions/run_detectionaccuracy.R"))
parallel::clusterEvalQ(cl = cl,  source("./functions/likelihood.R"))
parallel::clusterEvalQ(cl = cl,  source("./functions/calcpredictions.R"))

# lppds:
inputdata <- readRDS("./private/data/clean/7_2_1_input_data.rds")
Xocc = inputdata$holdoutdata$occ_covariates
yXobs = inputdata$holdoutdata$plotsmerged_detection
ModelSite = "ModelSiteID"

lppds <- lapply(filenames, function(x){
  fit <- readRDS(x)
  fit$data <- as.list.format(fit$data)
  lppd <- lppd.newdata(fit,
               Xocc = Xocc,
               yXobs = yXobs,
               ModelSite = "ModelSiteID",
               cl = cl)
  return(lppd)
})
saveRDS(lppds, file = "./tmpdata/7_2_2_lppds.rds")
parallel::stopCluster(cl)

# waics <- lapply(filenames, function(x){
#   fit <- readRDS(x)
#   likel.mat <- likelihoods.fit(fit,
#                cl = cl)
#   waic <- loo::waic(log(likel.mat))
#   looest <- loo::loo(log(likel.mat))
#   return(list(
#     waic = waic,
#     loo = looest
#   ))
# })
# saveRDS(lppds, file = "./tmpdata/7_2_2_lppds.rds")



stopCluster(cl)

