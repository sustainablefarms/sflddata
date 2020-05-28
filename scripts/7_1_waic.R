# Examples of computing WAIC for the 7_1 model examples
fit <- readRDS("./tmpdata/7_1_mcmcchain_20200424.rds")
fitdata <- list.format(fit$data)
library(dplyr); library(tidyr); library(tibble);
Xocc <- fitdata$Xocc %>%
  as_tibble() %>%
  rowid_to_column(var = "ModelSite") %>%
  nest(Xocc = -ModelSite)
Xobs <- fitdata$Xobs %>%
  as_tibble() %>%
  mutate(ModelSite = fitdata$ObservedSite) %>%
  nest(Xobs = -ModelSite)
y <- fitdata$y %>%
  as_tibble() %>%
  mutate(ModelSite = fitdata$ObservedSite) %>%
  nest(y = -ModelSite)
data <- inner_join(Xocc, Xobs, by = "ModelSite", suffix = c("occ", "obs")) %>%
  inner_join(y, by = "ModelSite", suffix = c("X", "y"))
nlv <- fitdata$nvl
numsims <- 1000
lvsim <- matrix(rnorm(nlv * numsims), ncol = 2, nrow = numsims) #simulated lv values, should average over thousands
draws <- fit$mcmc[[1]]

cl <- parallel::makeCluster(10)
system.time(waic <- loo::waic(pdetect_joint_marginal.data_i,
                              data = data,
                              draws = draws,
                              lvsim = lvsim,
                              cl = cl))
parallel::stopCluster(cl)
saveRDS(waic, "tmpdata/7_1_20200424_waic.rds")
#above took 20 minutes of user time on Wade's machine (10 cores)

system.time(looest <- loo::loo(pdetect_joint_marginal.data_i,
                               data = data,
                               draws = draws,
                               lvsim = lvsim,
                               cores = 10))

