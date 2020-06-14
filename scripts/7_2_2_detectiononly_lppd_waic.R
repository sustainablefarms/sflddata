filenames <- list(
wind = "./tmpdata_deto_wind.rds",
wind_June4 =  "./tmpdata/deto_wind_June4.rds",
time = "./tmpdata_deto_time.rds",
windtime = "./tmpdata_deto_windtime.rds",
windtemp = "./tmpdata_deto_windtemp.rds",
windtemp_time = "./tmpdata_deto_windtemp_time.rds",
windtimetemp_clouds = "./windtimetemp_clouds.rds",
windtimeclouds_temp = "./windtimeclouds_temp_June4.rds",
windtimecloudstemp = "./windtimecloudstema_June4.rds")

devtools::document()
devtools::install(upgrade = "never")
library(sustfarmld)

library(sustfarmld); library(dplyr); library(tibble); library(tidyr);
cl <- parallel::makeCluster(15)
as_list_format <- function(data, checkvalid = TRUE){
  if ("list" %in% class(data)){return(data)}
  out <- runjags::list.format(data, checkvalid = checkvalid)
  return(out)
}
parallel::clusterExport(cl = cl,  "as_list_format")
# parallel::clusterEvalQ(cl = cl,  source("./R/likelihood.R"))
# parallel::clusterEvalQ(cl = cl,  source("./R/calcpredictions.R"))

# lppds:
inputdata <- readRDS("./private/data/clean/7_2_1_input_data.rds")
Xocc = inputdata$holdoutdata$occ_covariates
yXobs = inputdata$holdoutdata$plotsmerged_detection
ModelSite = "ModelSiteID"

lppds <- lapply(filenames, function(x){
  fit <- readRDS(x)
  fit$data <- as_list_format(fit$data)
  colnames(fit$data$y) <- fit$species
  lppd <- lppd.newdata(fit,
               Xocc = Xocc,
               yXobs = yXobs,
               ModelSite = "ModelSiteID",
               cl = cl)
  return(lppd)
})
saveRDS(lppds, file = "./tmpdata/7_2_2_lppds.rds")
parallel::stopCluster(cl)

cl <- parallel::makeCluster(15)
waics <- lapply(filenames[[1]], function(x){
  fit <- readRDS(x)
  fit$data <- as_list_format(fit$data)
  colnames(fit$data$y) <- fit$species
  likel.mat <- likelihoods.fit(fit,
               cl = cl)
  waic <- loo::waic(log(likel.mat))
  looest <- loo::loo(log(likel.mat))
  return(list(
    waic = waic,
    loo = looest
  ))
})
saveRDS(waics, file = "./tmpdata/7_2_2_waics.rds")
stopCluster(cl)

