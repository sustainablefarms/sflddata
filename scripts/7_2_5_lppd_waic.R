filenames <- list(
  msnm_time = "./tmpdata/grnd_msnm_time_nolv.rds",
  os_msnm_gc_time = "./tmpdata/grnd_os_msnm_gc_time_nolv.rds",
  os_msnm_gc_time_wind = "./tmpdata/grnd_os_msnm_gc_time_wind_nolv.rds"
)

# test loading models
a <- vapply(filenames, file.exists, FUN.VALUE = FALSE)
stopifnot(all(a))


devtools::load_all()
library(dplyr); library(tibble); library(tidyr);
cl <- parallel::makeCluster(2)


# lppds:
inputdata <- readRDS("./private/data/clean/7_2_4_input_data.rds")
Xocc = inputdata$holdoutdata$Xocc
yXobs = inputdata$holdoutdata$yXobs
ModelSite = "ModelSiteID"

# lpds <- pbapply::pblapply(filenames, function(x){
#   fit <- readRDS(x)
#   fit$data <- as_list_format(fit$data)
#   # Start the clock!
#   ptm <- proc.time()
#   
#   lppd <- lppd.newdata(fit,
#                        Xocc = Xocc,
#                        yXobs = yXobs,
#                        ModelSite = "ModelSiteID",
#                        cl = cl)
#   
#   # Stop the clock
#   timetaken <- proc.time() - ptm
#   return(c(lppd, timetaken))
# })
# saveRDS(lpds, file = "./tmpdata/7_2_5_lpds.rds")
# parallel::stopCluster(cl)

cl <- parallel::makeCluster(2)
waics <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  # Start the clock!
  ptm <- proc.time()

  likel.mat <- likelihoods.fit(fit,
                               cl = cl)
  waicmsgs <- capture.output(waic <- loo::waic(log(likel.mat)))
  loomsgs <- capture.output(looest <- loo::loo(log(likel.mat), cores = length(cl)))

  # Stop the clock
  timetaken <- proc.time() - ptm

  out <- list(
    waic = waic,
    loo = looest,
    timetaken = timetaken,
    waicmsgs = waicmsgs,
    loomsgs = loomsgs
  )
  save(out, file = paste0("./tmpdata/WAICS/", basename(x)))

  return(out)
})
saveRDS(waics, file = "./tmpdata/7_2_5_waics.rds")
parallel::stopCluster(cl)

loo_warnings <- warnings()
saveRDS(loo_warnings, file = "./tmpdata/WAICS/7_2_5_warnings.rds")