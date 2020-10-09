fit <- readRDS("~/Documents/professional/ANU/linking-data-project/Experiments/7_4_modelrefinement/fittedmodels/7_4_11_LV_model_2lv.rds")
# sort(vapply(fit, function(x) object.size(x), FUN.VALUE = object.size(fit))) * 1E-6
# object.size(fit) * 1E-6

fit$samplers <- NULL
fit$mcse <- NULL

# slim down number of sites
selmodelsites <- 1:10  #must be consecutive from 1 onwards for the new ModelSite map to apply to the ROWS of Xocc
fit$data$Xocc <- fit$data$Xocc[selmodelsites, ]
fit$data$y <- fit$data$y[fit$data$ModelSite %in% selmodelsites, ]
fit$data$Xobs <- fit$data$Xobs[fit$data$ModelSite %in% selmodelsites, ]
fit$data$Vvisits <- nrow(fit$data$Xobs) 
fit$data$J <- length(selmodelsites)
fit$data$ModelSite <- fit$data$ModelSite[fit$data$ModelSite %in% selmodelsites]
stopifnot(all(fit$data$ModelSite %in% 1:nrow(fit$data$Xocc)))

# randomise site latitude by 70km - kept close so that model predictions are ok. Note that longitude is not available.
fit$data$Xocc[, "latitude"] <- fit$data$Xocc[, "latitude"] + runif(length(selmodelsites), min = -1, max = 1)


#slim down mcmc according to number of sites
LVcolkeep <- lapply(selmodelsites, function(x) grepl(paste0("LV\\[", x, ","), colnames(fit$mcmc[[1]])))
colkeep <- Reduce("|", LVcolkeep) | !grepl("^LV\\[", colnames(fit$mcmc[[1]]))
fit$mcmc[[1]] <- fit$mcmc[[1]][, colkeep]

#fewer mcmc samples
fit$mcmc[[1]] <- fit$mcmc[[1]][seq(1, nrow(fit$mcmc[[1]]), by = 30), ]

fit$end.state <- NULL

# remove any posterior stuff because mcmc samples have been removed.
fit$quality <- NULL
fit$summaries <- NULL
fit$summary <- NULL
fit$hpd <- NULL
fit$HPD <- NULL
fit$autocorr <- NULL
fit$discrete <- NULL
fit$nonstochastic <- NULL
fit$semistochastic <- NULL
fit$truestochastic <- NULL

# cut down tables based on fewer sites (obsolete as mcmc samples cut)
# fit$summary$statistics <- fit$summary$statistics[colnames(fit$mcmc[[1]]), ]
# fit$summary$quantiles <- fit$summary$quantiles[colnames(fit$mcmc[[1]]), ]
# fit$summaries <- fit$summaries[colnames(fit$mcmc[[1]]), ]

saveRDS(fit, file = "./private/data/testdata/cutfit_7_4_11_2LV.rds")
