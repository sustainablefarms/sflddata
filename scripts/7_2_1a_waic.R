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
nlv <- fitdata$nvl
numsims <- 1000
lvsim <- matrix(rnorm(nlv * numsims), ncol = 2, nrow = numsims) #simulated lv values, should average over thousands
draws <- fit$mcmc[[1]]

cl <- parallel::makeCluster(30)
system.time(waic <- loo::waic(pdetect_joint_marginal.data_i,
                              data = data,
                              draws = draws,
                              lvsim = lvsim,
                              cl = cl))
parallel::stopCluster(cl)
saveRDS(waic, "tmpdata/7_2_1a_20200524_waic.rds")
#above took 20 minutes of user time on Wade's machine (10 cores)

system.time(looest <- loo::loo(pdetect_joint_marginal.data_i,
                               data = data,
                               draws = draws,
                               lvsim = lvsim,
                               cores = 30))
saveRDS(looest, "tmpdata/7_2_1a_20200524_loo.rds")
