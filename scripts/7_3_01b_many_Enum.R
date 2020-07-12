# calculating the expected number of species for many models. Insample and holdout data

library(sustfarmld)
library(tidyr)
library(dplyr)

modelspecs <- c(
  readRDS("./tmpdata/7_2_10_modelspecs.rds"),
  readRDS("./tmpdata/7_2_11_modelspecs.rds"),
  readRDS("./tmpdata/7_2_12_modelspecs.rds"),
  readRDS("./tmpdata/7_2_13_modelspecs.rds"),
  readRDS("./tmpdata/7_3_00_modelspecs.rds"),
  readRDS("./tmpdata/7_3_01_modelspecs.rds")
    )

filenames <- lapply(modelspecs, function(x) x$filename)

# test loading models
a <- vapply(filenames, file.exists, FUN.VALUE = FALSE)
stopifnot(all(a))

cl <- parallel::makeCluster(20)
inputdata <- readRDS("./private/data/clean/7_2_10_input_data.rds")
parallel::clusterEvalQ(cl, library(sustfarmld))
holdout_prednumbers_l <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  # Start the clock!
  ptm <- proc.time()
  prednumbers <- 
    predsumspecies_newdata(fit,
                           inputdata$holdoutdata$Xocc,
                           inputdata$holdoutdata$yXobs,
                           ModelSiteVars = "ModelSiteID") 
  # Stop the clock
  timetaken <- proc.time() - ptm
  
  return(prednumbers)
})
saveRDS(holdout_prednumbers_l, "./tmpdata/7_3_01b_many_Enum_holdout.rds")


insample_prednumbers_l <- pbapply::pblapply(filenames, function(x){
  # prep object
  fit <- readRDS(x)
  # Start the clock!
  ptm <- proc.time()
  
  prednumbers <- predsumspecies(fit, UseFittedLV = !is.null(fit$data$nlv), cl = cl)

  # Stop the clock
  timetaken <- proc.time() - ptm
  print(timetaken)

  return(prednumbers)
})
saveRDS(insample_prednumbers_l, "./tmpdata/7_3_01b_many_Enum_insample.rds")

