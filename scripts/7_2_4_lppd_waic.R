filenames <- list(
  os = "./tmpdata/grnd_os_nolv.rds",
  ms = "./tmpdata/grnd_os_ms_nolv.rds",
  os_gc = "./tmpdata/grnd_os_gc_nolv.rds",
  nm = "./tmpdata/grnd_nm_nolv.rds",
  msnm = "./tmpdata/grnd_msnm_nolv.rds",
  pars = "./tmpdata/grnd_pars_nolv.rds"
)

# test loading models
a <- vapply(filenames, file.exists, FUN.VALUE = FALSE)
stopifnot(all(a))


devtools::load_all()
library(dplyr); library(tibble); library(tidyr);
cl <- parallel::makeCluster(10)


# lppds:
inputdata <- readRDS("./private/data/clean/7_2_4_input_data.rds")
Xocc = inputdata$holdoutdata$Xocc
yXobs = inputdata$holdoutdata$yXobs
ModelSite = "ModelSiteID"

lpds <- pbapply::pblapply(filenames, function(x){
  fit <- readRDS(x)
  fit$data <- as_list_format(fit$data)
  # Start the clock!
  ptm <- proc.time()
  
  lppd <- lppd.newdata(fit,
                       Xocc = Xocc,
                       yXobs = yXobs,
                       ModelSite = "ModelSiteID",
                       cl = cl)
  
  # Stop the clock
  timetaken <- proc.time() - ptm
  return(c(lppd, timetaken))
})
saveRDS(lpds, file = "./tmpdata/7_2_4_lpds.rds")
parallel::stopCluster(cl)

cl <- parallel::makeCluster(33)
waics <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  # Start the clock!
  ptm <- proc.time()
  
  likel.mat <- likelihoods.fit(fit,
                               cl = cl)
  waic <- loo::waic(log(likel.mat))
  looest <- loo::loo(log(likel.mat), cores = length(cl))
  
  # Stop the clock
  timetaken <- proc.time() - ptm
  
  out <- list(
    waic = waic,
    loo = looest,
    timetaken = timetaken
  )
  save(out, file = paste0("./tmpdata/WAICS/", basename(x)))
  
  return(out)
})
saveRDS(waics, file = "./tmpdata/7_2_4_waics.rds")
parallel::stopCluster(cl)

loo_warnings <- warnings()
saveRDS(loo_warnings, file = "./tmpdata/WAICS/7_2_4_warnings.rds")