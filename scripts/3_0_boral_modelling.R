# analysis of SWS angry bird dataset

birds <- readRDS("./data/clean/sws_birds.rds")
bird_richness <- readRDS("./data/clean/sws_bird_richness.rds")
sites <- readRDS("./data/clean/sws_sites_rs.rds")
traits <- readRDS("./data/clean/sws_traits.rds")
# str(traits)
# str(birds)
# str(sites)


# organise bird data
# make binary
for(i in 1:ncol(birds)){
  birds[which(birds[, i] > 0), i] <- 1
}
for(i in 1:ncol(bird_richness)){
  bird_richness[which(bird_richness[, i] > 0), i] <- 1
}


# make predictor matrix
sites_rs$row_order <- seq_len(nrow(sites_rs))
sites_list <- split(sites_rs[, c("row_order", "gpp")], sites_rs$SiteCode)
sites_list <- lapply(sites_list, function(a){
  mean_val <- mean(a$gpp, na.rm = TRUE)
  a$gpp_sitemean <- mean_val
  a$gpp_sitedev <- a$gpp - mean_val
  return(a)
})
sites_extra <- do.call(rbind, sites_list)
sites_extra <- sites_extra[order(sites_extra$row_order), ]
sites_rs$gpp_mean <- sites_extra$gpp_sitemean
sites_rs$gpp_dev <- sites_extra$gpp_sitedev

predictors <- data.frame(
  # type = sites$GrowthType,
  gpp_mean = scale(sites_rs$gpp_mean),
  gpp_dev = scale(sites_rs$gpp_dev),
  fmc = scale(sites_rs$fmc),
  year = scale(as.numeric(sites_rs$date))
)
# plot(gpp_mean ~ fmc, data = predictors)

na_check <- which(apply(
  cbind(predictors, birds),
  1,
  function(a){all(!is.na(a))}
))
# length(which(na_check))

predictors <- predictors[na_check, ]
X <- model.matrix( ~ fmc + gpp_mean + gpp_dev + year, data = predictors)[, -1]
# plot(predictors)

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

# to fit mass to all predictors
T_which <- rep(0, ncol(X)+1)
T_which[seq_along(T_which)] <- 1
T_which <- as.list(T_which)
# leave this for later

# to customise trait/variable combinations, use code of this form:
# T_which <- list(
#   c(0), # intercept
# )

# or jsut fit all predictors to all traits
nX <- ncol(X)
nT <- ncol(T)
T_which <- split(
  rep(seq_len(nT), nX+1),
  rep(seq_len(nX+1), each = nT)
)


# model
library(boral)

# quick
mcmc_control_test <- list(
  n.burnin = 1,
  n.iteration = 20,
  n.thin = 1
)

# intermediate
mcmc_control_ok <- list(
  n.burnin = 100,
  n.iteration = 2000,
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
  traits = T,
  which.traits = T_which,
  family = "binomial",
  lv.control = list(num.lv = 2),
  row.eff = "random", # i.e. we have repeat visits to each site
  row.ids = R,
  save.model = TRUE, # necessary for get.residual.cor
  model.name = "boral_model_test.txt",
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
ggsave("example_traits_2_Nov2019.pdf")


## PLOT COEFFICIENTS
# create function to reorder rows of a coefsplot
# move this to boralis soon
order_boral <- function(
  data,  # data.frame from boral_coefs
  x # a name of a covariate in data
){
  data_list <- split(data, data$covname)
  data_order <- order(data_list[[x]]$x, decreasing = TRUE)
  data_ordered <- lapply(data_list, function(a, order){
    result <- a[order, ]
    result$labels <- as.character(result$labels)
    result$labels <- factor(
      seq_len(nrow(result)),
      levels = seq_len(nrow(result)),
      labels = result$labels
    )
    return(result)
  }, order = data_order)
  data_final <- do.call(rbind, data_ordered)
  return(data_final)
}


boral_data <- order_boral(boral_coefs(model), "gpp_mean")
boral_coefsplot(boral_data) +
  facet_wrap(
    facets = ~covname,
    nrow = 1,
    ncol = 4,
    scales = "free_x"
  ) +
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
ggsave("boral_coefsplot_2_Nov2019.pdf")


# PLOT LATENT VARIABLES (ordination)
library(ggplot2)
ordination_data <- as.data.frame(model$lv.coefs.median)
ordination_data$taxon <- rownames(ordination_data)
rownames(ordination_data) <- NULL
ggplot(ordination_data, aes(x = theta1, y = theta2, label = taxon)) +
  geom_text(size = 3) +
  expand_limits(x = c(-1, 1))
ggsave("./plots/boral_lvsplot.pdf")

# correlations
resid_cor <- get.residual.cor(model)
enviro_cor <- get.enviro.cor(model)

# install.packages("corrplot")
library(corrplot)
corrplot(resid_cor$sig.cor, title = "Residual correlations",
  type = "lower", diag = FALSE, tl.srt = 45, tl.col = "grey30")

corrplot(enviro_cor$sig.cor, title = "Environmental correlations",
  type = "lower", diag = FALSE, tl.srt = 45, tl.col = "grey30")