# analysis of SWS angry bird dataset with m7 coefficients as covariates
## NEVER FULLY RUN!  Decided coefficients of m7 was not useful for broad prediction. ##
library(boral)
library(dplyr); library(fable); library(tidyr)
source("./functions/order_boral.R")
source("./functions/return_current_time.R") # necessary function for file out


# import data
bird_richness <- readRDS("./private/data/clean/sws_bird_richness.rds") # contains all bird spp
birds <- readRDS("./private/data/clean/sws_birds.rds") # contains only common bird spp
sites_rs <- readRDS("./private/data/clean/sws_sites_rs.rds")
  sites_rs$log_plus_one_woody_cover <- log(sites_rs$woody_cover + 1)
traits <- readRDS("./private/data/clean/sws_traits.rds")

## format m7 coefficients for each site
m7 <- readRDS("./private/models/m7_v1.rds")
m7_farmterms <- tidy(m7) %>%
  filter(grepl("farm", term)) %>%
  mutate(termshort = gsub("farm....", "", term)) %>%
  mutate(termshort = gsub("^$", "intercept:", termshort)) %>%
  mutate(farm = substring(term, regexpr("farm", term) + 4, regexpr("farm....", term) +  7)) %>%
  select(estimate, termshort, farm) %>%
  pivot_wider(id_cols = farm, names_from = termshort, values_from = estimate) %>%
  rbind(rep(0, 9)) %>% #ARCH coefficients are 0 by default
  mutate(farm = if_else(grepl("0", farm), "ARCH", farm))

sites_rs <- sites_rs %>%
  mutate(farm = substr(SiteCode, 1, 4)) %>% #add a farm column to make possible to join m7 coefficient estimates
  left_join(m7_farmterms, by = "farm", m7_siteterms)
  
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


# make predictor matrix
predictors_prelim <- list(
  # type = sites$GrowthType,
  # noisy_miners = birds[, nm_col],
  gpp_mean = scale(sites_rs$gpp_mean),
  gpp_diff = scale(sites_rs$gpp_diff),
  fmc_mean = scale(sites_rs$fmc_mean),
  fmc_diff = scale(sites_rs$fmc_diff),
  woody_cover = scale(sites_rs$log_plus_one_woody_cover),
  year = scale(as.numeric(sites_rs$date)),
  m7_rainindrierseasonA = scale(sites_rs$`I(1/pg_1to5m.ydaymed):pg_24d:`)
  #m7_rainindrierseasonB = scale(sites_rs$`pg_1to5m:I(1/pg_1to5m.ydaymed):`)
)
scale_list <- lapply(predictors_prelim,
  function(x) as.numeric(attributes(x)[2:3]))
scale_matrix <- do.call(cbind, scale_list)
rownames(scale_matrix) <- c("centre", "scale")
saveRDS(scale_matrix, "./private/coefficients/4_0_script_scale_matrix.rds")

predictors <- data.frame(predictors_prelim)
# plot(predictors)


# remove rows that have NA:
na_check <- which(apply(
  cbind(predictors, birds),
  1,
  function(a){all(!is.na(a))}
))
stopifnot(length(na_check) == 2018)  #2018 is what length has been in the past
predictors <- predictors[na_check, ]

# build model matrix
X <- model.matrix(
  ~ gpp_mean * woody_cover + fmc_diff + gpp_diff + year + m7_rainindrierseasonA,
  data = predictors)[, -1]
# NOTE: gpp_mean and fmc_mean are correlated

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
  # traits = T,
  # which.traits = T_which,
  family = "binomial",
  lv.control = list(num.lv = 2),
  row.eff = "random", # i.e. we have repeat visits to each site
  row.ids = R,
  save.model = TRUE, # necessary for get.residual.cor
  model.name = "./JAGS/boral_model_test.txt",
  mcmc.control = mcmc_control_test
)
saveRDS(model, "./private/models/boral_model_2020-02-03.rds")
