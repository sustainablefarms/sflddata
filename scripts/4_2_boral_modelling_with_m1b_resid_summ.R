library(boral)
library(dplyr); library(tidyr)
source("./R/order_boral.R")
source("./R/return_current_time.R") # necessary function for file out


# import data
bird_richness <- readRDS("./private/data/clean/sws_bird_richness.rds") # contains all bird spp
birds <- readRDS("./private/data/clean/sws_birds.rds") # contains only common bird spp
sites_rs <- readRDS("./private/data/clean/sws_sites_rs.rds")
  sites_rs$log_plus_one_woody_cover <- log(sites_rs$woody_cover + 1)
traits <- readRDS("./private/data/clean/sws_traits.rds")
m1bresids_summaries <- readRDS("./private/models/m1b_resids_summaries.rds")
sites_rs <- sites_rs %>%
  mutate(farm = substr(SiteCode, 1, 4)) %>%
  left_join(m1bresids_summaries, by = "farm")

# organise bird data
# make binary
for(i in 1:ncol(birds)){
  birds[which(birds[, i] > 0), i] <- 1
}
for(i in 1:ncol(bird_richness)){
  bird_richness[which(bird_richness[, i] > 0), i] <- 1
}

# nm_col <- which(colnames(birds) == "Noisy Miner")
# birds <- birds[, -nm_col]

# make matrix of possible predictors
predictors <- model.frame(~ gpp_mean +
                            log_plus_one_woody_cover + 
                            date + 
                            m1b_resid + mean + med + iqr + q10 + q90, data = sites_rs, na.action = NULL)
# NOTE: gpp_mean and fmc_mean are correlated; m1b_resid and gpp_diff are correlated; and fmc_diff hasn't seemed that useful

## scale them
predictors <- predictors %>%
  mutate(date = as.numeric(date)) %>%
  scale()
scale_matrix <- data.frame(attributes(predictors)[c("scaled:center", "scaled:scale")]) %>%
  rename(center = "scaled.center", scale = "scaled.scale") %>%
  t()
saveRDS(scale_matrix, "./private/coefficients/4_2_script_scale_matrix.rds")

# plot predictors to check correlation determine useful ones
plot(data.frame(predictors))

# remove rows that have NA:
na_check <- which(apply(
  cbind(predictors, birds),
  1,
  function(a){all(!is.na(a))}
))
stopifnot(length(na_check) == 2032)  #2032 is new length because fmc_diff isn't included
predictors <- predictors[na_check, ]

X <- model.matrix(~ gpp_mean * log_plus_one_woody_cover + date + m1b_resid + q90,
                  data.frame(predictors))[, -1]
# NOTE: gpp_mean and fmc_mean are correlated; m1b_resid and gpp_diff are correlated; and fmc_diff hasn't seemed that useful

# make Y matrix
Y <- as.matrix(birds[na_check, ])
keep_birds <- colSums(Y) > 70
Y <- Y[, keep_birds]

Y_richness <- as.matrix(bird_richness[na_check, ])

# random effects
R <- matrix(as.numeric(as.factor(sites_rs$SiteCode[na_check])), ncol = 1)

# make trait matrix
# colnames(Y) %in% traits$common_name # all true
traits <- traits[traits$common_name %in% colnames(Y), ]
# all(traits$common_name == colnames(Y))# TRUE
# xtabs(~traits$diet)
T <- model.matrix(~ scale(lnmass) + diet + movements, data = traits)[, -1]
colnames(T)[1] <- "mass"

# to customise trait/variable combinations, use code of this form:
# T_which <- list(
#   c(0), # intercept
# )

# or just fit all predictors to all traits
nX <- ncol(X)
nT <- ncol(T)
T_which <- split(
  rep(seq_len(nT), nX+1),
  rep(seq_len(nX+1), each = nT)
)


# run boral model

# quick
mcmc_control_test <- list(
  n.burnin = 1,
  n.iteration = 20,
  n.thin = 1
)

# intermediate
mcmc_control_ok <- list(
  n.burnin = 300,
  n.iteration = 3000,
  n.thin = 20
)

# default
mcmc_control_default <- list(
  n.burnin = 10000,
  n.iteration = 40000,
  n.thin = 30
)

model <- boral(
  y = Y,
  X = X,
  family = "binomial",
  lv.control = list(num.lv = 2),
  row.eff = "random", # i.e. we have repeat visits to each site
  row.ids = R,
  save.model = TRUE, # necessary for get.residual.cor
  model.name = "./JAGS/boral_model_ok_2020-02-08.txt",
  mcmc.control = mcmc_control_ok
)
saveRDS(model, "./private/models/boral_model_2020-02-09_m1b.rds")
## Fitting cost:
# Graph information:
#    Observed stochastic nodes: 134112
#    Unobserved stochastic nodes: 4834
#    Total graph size: 827805
