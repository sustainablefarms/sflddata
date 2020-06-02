# Examples of computing WAIC for the 7_1 model examples
library(dplyr); library(tidyr); library(tibble); library(runjags)
source("./functions/loo.occdet.R")
fit <- readRDS("./tmpdata/7_2_1_mcmcchain_20200524.rds")
fitdata <- list.format(fit$data)
Xocc <- fitdata$Xocc %>%
  as_tibble(.name_repair = "universal") %>%
  rowid_to_column(var = "ModelSite") %>%
  nest(Xocc = -ModelSite)
Xobs <- fitdata$Xobs %>%
  as_tibble(.name_repair = "universal") %>%
  mutate(ModelSite = fitdata$ModelSite) %>%
  nest(Xobs = -ModelSite)
y <- fitdata$y %>%
  as_tibble(.name_repair = "universal") %>%
  mutate(ModelSite = fitdata$ModelSite) %>%
  nest(y = -ModelSite)
data <- inner_join(Xocc, Xobs, by = "ModelSite", suffix = c("occ", "obs")) %>%
  inner_join(y, by = "ModelSite", suffix = c("X", "y"))
datalist <- lapply(1:nrow(data), function(i) data[i,, drop = FALSE])
nlv <- fitdata$nlv
numsims <- 1000
lvsim <- matrix(rnorm(nlv * numsims), ncol = 2, nrow = numsims) #simulated lv values, should average over thousands
draws <- fit$mcmc[[1]]

stopifnot(is.finite(pdetect_joint_marginal.ModelSite(datalist[[1]]$Xocc[[1]],
                                 datalist[[1]]$Xobs[[1]],
                                 datalist[[1]]$y[[1]],
                                 theta = draws[1, ],
                                 lvsim = lvsim)))

cl <- parallel::makeCluster(15)
parallel::clusterExport(cl, list("pdetect_joint_marginal.data_i",
                                 "pdetect_joint_marginal.ModelSite",
                                 "bugsvar2array"))
system.time(llvals <- parallel::parLapply(cl,
                              X = datalist,
                              fun = pdetect_joint_marginal.data_i,
                              draws = draws,
                              lvsim = lvsim))
# 4588 seconds elapsed (76 minutes)
parallel::stopCluster(cl)

names(llvals) <- data$ModelSite
saveRDS(llvals, "tmpdata/7_2_1a_20200524_llvals.rds")

llvals.mat <- simplify2array(llvals, higher = FALSE)
waic <- loo::waic(llvals.mat)
looest <- loo::loo(llvals.mat)
