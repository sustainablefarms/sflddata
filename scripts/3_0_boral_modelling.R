# analysis of SWS angry bird dataset
library(boral)
source("./functions/order_boral.R")
source("./functions/return_current_time.R") # necessary function for file out


# import data
bird_richness <- readRDS("./private/data/clean/sws_bird_richness.rds") # contains all bird spp
birds <- readRDS("./private/data/clean/sws_birds.rds") # contains only common bird spp
sites_rs <- readRDS("./private/data/clean/sws_sites_rs.rds")
traits <- readRDS("./private/data/clean/sws_traits.rds")

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
predictors <- data.frame(
  # type = sites$GrowthType,
  # noisy_miners = birds[, nm_col],
  gpp_mean = scale(sites_rs$gpp_mean),
  gpp_diff = scale(sites_rs$gpp_diff),
  fmc_mean = scale(sites_rs$fmc_mean),
  fmc_diff = scale(sites_rs$fmc_diff),
  woody_cover = scale(log(sites_rs$woody_cover + 1)),
  year = scale(as.numeric(sites_rs$date))
)
# plot(predictors)

na_check <- which(apply(
  cbind(predictors, birds),
  1,
  function(a){all(!is.na(a))}
))
# length(na_check)

predictors <- predictors[na_check, ]
X <- model.matrix(
  # ~ noisy_miners * fmc_mean + noisy_miners * gpp_mean + fmc_diff + gpp_diff + year,
  ~ gpp_mean * woody_cover + fmc_diff + gpp_diff + year,
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
  mcmc.control = mcmc_control_ok
)


# PLOT TRAITS
# devtools::install_github("mjwestgate/boralis")
library(boralis)
library(ggplot2)

# boral_coefsplot(model, type = "traits")

# extract and draw manually
traits_data <- boral_coefs(model, type = "traits")
traits_data <- traits_data[!(traits_data$covname %in% c("kappa0", "sigma")), ]
# traits_data <- traits_data[traits_data$covname != "sigma", ]
# traits_data <- traits_data[traits_data$labels != "beta0", ]
levels(traits_data$labels)[1] <- "intercept"

ggplot(traits_data, aes(x = x, y = labels, color = overlaps_zero)) +
  scale_y_discrete() + geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = x0, xmax = x1)) +
  geom_point() +
  scale_colour_manual(values = c("black", "grey70")) +
  theme_bw() +
  guides(color = FALSE) +
  ylab("Variable") +
  facet_wrap(~covname, scales = "free_x") +
  xlab("Trait Coefficient")

ggsave(
  paste0(
    "./private/plots/boral_coefsplot_traits_",
    return_current_time(),
    ".pdf"
  )
)


## EXPORT COEFFICIENTS
# model_summary <- summary(model)
# model_summary$coefficients

# as a data.frame
boral_results <- boral_coefs(model)[, c(1, 3, 4)]
colnames(boral_results) <- c("predictor", "species", "median_coefficient")
boral_results$species <- as.character(boral_results$species)

intercepts <- data.frame(
  predictor = "intercept",
  species = rownames(model$lv.coefs.median),
  median_coefficient = model$lv.coefs.median[, 1],
  stringsAsFactors = FALSE
)

boral_results <- rbind(intercepts, boral_results)
saveRDS(boral_results, "./private/data/coefficients/boral_coefficients_dataframe.rds")

boral_matrix <- as.matrix(do.call(
  cbind,
  split(boral_results$median_coefficient, boral_results$predictor)
))
boral_matrix <- boral_matrix[, c(5, 3, 2, 1, 6, 4, 7)]
rownames(boral_matrix) <- intercepts$species
saveRDS(boral_matrix, "./private/data/coefficients/boral_coefficients_matrix.rds")


## PLOT COEFFICIENTS
boral_data <- order_boral(boral_coefs(model), "gpp_mean")
boral_coefsplot(boral_data) +
  # facet_wrap( # to manually override subplot arrangement
  #   facets = ~covname,
  #   # nrow = 1,
  #   # ncol = 4,
  #   scales = "free_x"
  # ) +
  theme(
    axis.text = element_text(size = 6),
    strip.background = element_rect(
      fill = "white",
      color = "white"),
    strip.text = element_text(
      hjust = 0,
      size = 8
    )
  )


# export using current date and time in file path
ggsave(
  paste0(
    "./private/plots/boral_coefsplot_",
    return_current_time(),
    ".pdf"
  )
)


# PLOT LATENT VARIABLES (ordination)
library(ggplot2)
ordination_data <- as.data.frame(model$lv.coefs.median)
ordination_data$taxon <- rownames(ordination_data)
rownames(ordination_data) <- NULL
ggplot(ordination_data, aes(x = theta1, y = theta2, label = taxon)) +
  geom_text(size = 3) +
  expand_limits(x = c(-1, 1))
ggsave("./private/plots/boral_lvsplot.pdf")

# correlations
resid_cor <- get.residual.cor(model)
enviro_cor <- get.enviro.cor(model)

# install.packages("corrplot")
library(corrplot)
corrplot(resid_cor$sig.cor, title = "Residual correlations",
  type = "lower", diag = FALSE, tl.srt = 45, tl.col = "grey30")

corrplot(enviro_cor$sig.cor, title = "Environmental correlations",
  type = "lower", diag = FALSE, tl.srt = 45, tl.col = "grey30")

# question over whether woody cover changes much at the site level
# and therefore whether it is worth splitting into spatial and temporal components

# is fmc useful at all?
# should gpp_diff:woody_cover be a term?