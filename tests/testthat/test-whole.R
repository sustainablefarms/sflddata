# test that the following are consistent: 
# simulation data, runjags MCMC (posterior distribution matches true), predicted likelihoods, expected species detections, DS residuals

context("Wholistic tests using identical, independent, ModelSites")

# Create a process with known parameters
artmodel <- artificial_runjags(nspecies = 2, nsites = 4000, nvisitspersite = 2, nlv = 0,
                               ObsFmla = "~ 1",
                               OccFmla = "~ 1")

# fit to data and simulations using runjags
originalXocc <- Rfast::eachrow(Rfast::eachrow(artmodel$data$Xocc, artmodel$XoccProcess$scale, oper = "*"),
                               artmodel$XoccProcess$center, oper = "+")
colnames(originalXocc) <- colnames(artmodel$data$Xocc)
originalXocc <- cbind(ModelSite = 1:nrow(originalXocc), originalXocc)
originalXobs <- Rfast::eachrow(Rfast::eachrow(artmodel$data$Xobs, artmodel$XobsProcess$scale, oper = "*"),
                               artmodel$XobsProcess$center, oper = "+")
colnames(originalXobs) <- colnames(artmodel$data$Xobs)
originalXobs <- cbind(ModelSite = artmodel$data$ModelSite, originalXobs)

fit_runjags <- run.detectionoccupancy(originalXocc, cbind(originalXobs, artmodel$data$y), 
                       species = colnames(artmodel$data$y),
                       ModelSite = "ModelSite",
                       OccFmla = artmodel$XoccProcess$fmla,
                       ObsFmla = artmodel$XobsProcess$fmla,
                       initsfunction = function(chain, indata){return(NULL)},
                       nlv = 0)

save(fit_runjags, artmodel, originalXocc, originalXobs, file = "./tests/testthat/benchmark_identicalsitesmodel.Rdata")

test_that("Posterior credible distribution overlaps true parameters", {
  var2compare <- colnames(artmodel$mcmc[[1]])
  expect_true(all(fit_runjags$summaries[var2compare, "Lower95"] <= artmodel$mcmc[[1]][1, var2compare]))
  expect_true(all(fit_runjags$summaries[var2compare, "Upper95"] >= artmodel$mcmc[[1]][1, var2compare]))
})

test_that("Predicted likelihoods match observations", {
  my <- cbind(ModelSite = fit_runjags$data$ModelSite, fit_runjags$data$y)
  obs_per_site <- lapply(1:nrow(fit_runjags$data$Xocc), function(x) my[my[, "ModelSite"] == x, -1])
  jointoutcomes <- vapply(obs_per_site, paste0, collapse = ",", FUN.VALUE = "achar")
  
  # extra, fake observations for more accurate simulated likelihood
  makemoreobs <- function(){
    my_add <- cbind(ModelSite = fit_runjags$data$ModelSite, simulate_fit(artmodel, esttype = 1, UseFittedLV = FALSE))
    obs_per_site_add <- lapply(1:nrow(fit_runjags$data$Xocc), function(x) my_add[my_add[, "ModelSite"] == x, -1])
    jointoutcomes_add <- vapply(obs_per_site_add, paste0, collapse = ",", FUN.VALUE = "achar")
    return(jointoutcomes_add)
  }
  cl <- parallel::makeCluster(10)
  parallel::clusterExport(cl, c("makemoreobs", "fit_runjags", "artmodel"))
  parallel::clusterEvalQ(cl, library(sustfarmld))
  jointoutcomes_more <- pbapply::pbreplicate(1000, makemoreobs(), simplify = FALSE, cl = cl)
  
  # likelihood by simulation
  jointoutcomes_all <- c(jointoutcomes, unlist(jointoutcomes_more))
  sim_distr <- summary(factor(jointoutcomes_all))/length(jointoutcomes_all)
  lkl_sim <- sim_distr[jointoutcomes]
  
  # from this package
  lkl_runjags <- likelihoods.fit(fit_runjags, cl = cl)
  lkl_artmodel <- likelihoods.fit(artmodel, cl = cl)
  parallel::stopCluster(cl)
  
  # doublecheck likelihoods from artmodel
  expect_equivalent(Rfast::colmeans(lkl_artmodel), lkl_sim, tolerance = 0.01)
  
  # check likelihoods from runjags fit
  expect_equivalent(Rfast::colmeans(lkl_runjags), lkl_sim, tolerance = 0.01)
  expect_true(all(abs(Rfast::colmeans(lkl_runjags) - lkl_sim) / lkl_sim < 0.05))
  
  hist(abs(Rfast::colmeans(lkl_runjags) - lkl_sim) / lkl_sim)
  plt <- cbind(simlkl = lkl_sim,
               lklrunjags = Rfast::colmeans(lkl_runjags),
               lklartmodel = Rfast::colmeans(lkl_artmodel)) %>%
    dplyr::as_tibble(rownames = "Observations") %>%
    dplyr::mutate(ModelSiteID = 1:nrow(fit_runjags$data$Xocc)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x = ModelSiteID, y = lklrunjags)) +
    ggplot2::geom_point(ggplot2::aes(x = ModelSiteID, y = lklartmodel), col = "red", shape = "+") +
    ggplot2::geom_point(ggplot2::aes(x = ModelSiteID, y = simlkl), col = "blue", shape = "+") +
    # ggplot2::geom_point(ggplot2::aes(x = ModelSiteID, y = (lkl - simlkl) / simlkl), col = "blue", shape = "+") +
    ggplot2::scale_y_continuous(trans = "log10")
  print(plt)
})

test_that("Expected Number of Detected Species", {
  cl <- parallel::makeCluster(10)
  Enumspec <- predsumspecies(fit_runjags, UseFittedLV = FALSE, cl = cl)
  parallel::stopCluster(cl)
  NumSpecies <- detectednumspec(y = fit_runjags$data$y, ModelSite = fit_runjags$data$ModelSite)
  
  meandiff <- dplyr::cummean(NumSpecies - Enumspec["Esum_det", ])
  meanvar <- cumsum(Enumspec["Vsum_det", ])/((1:ncol(Enumspec))^2)
  plt <- cbind(diff = meandiff, var  = meanvar) %>% 
    dplyr::as_tibble(rownames = "CumSites") %>% 
    dplyr::mutate(CumSites = as.double(CumSites)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x= CumSites, ymin = -2 * sqrt(var), ymax = 2 * sqrt(var)), fill = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = CumSites, y = diff), col = "blue", lwd = 2)
  # print(plt)
  
  # check with predicted standard error once the software is computed
  sd_final <- sqrt(meanvar[ncol(Enumspec)])
  expect_equal(meandiff[ncol(Enumspec)], 0, tol = 3 * sd_final)
  
  Enum_compare_sum <- Enum_compare(NumSpecies,
                                   as.matrix(Enumspec["Esum_det", ], ncol = 1),
                                   as.matrix(Enumspec["Vsum_det", ], ncol = 1)
  )
  expect_equivalent(Enum_compare_sum[["E[D]_obs"]], 0, tol = 3 * Enum_compare_sum[["SE(E[D]_obs)_model"]])
  expect_equivalent(Enum_compare_sum[["E[D]_obs"]], 0, tol = 3 * Enum_compare_sum[["SE(E[D]_obs)_obs"]])
  expect_equivalent(Enum_compare_sum[["V[D]_model"]], Enum_compare_sum[["V[D]_obs"]], tol = 0.05 * Enum_compare_sum[["V[D]_obs"]])
  })

############### Wholistic Test on different ModelSites and LVs #########
# compare artmodel generated likelihood and Enum species to fit_runjags versions, and Enum species to observations

context("Wholistic tests on model with different ModelSites and LVs")

# Create a process with known parameters
artmodel <- artificial_runjags(nspecies = 10, nsites = 4000, nvisitspersite = 2, nlv = 4)

# fit to data and simulations using runjags
originalXocc <- Rfast::eachrow(Rfast::eachrow(artmodel$data$Xocc, artmodel$XoccProcess$scale, oper = "*"),
                               artmodel$XoccProcess$center, oper = "+")
colnames(originalXocc) <- colnames(artmodel$data$Xocc)
originalXocc <- cbind(ModelSite = 1:nrow(originalXocc), originalXocc)
originalXobs <- Rfast::eachrow(Rfast::eachrow(artmodel$data$Xobs, artmodel$XobsProcess$scale, oper = "*"),
                               artmodel$XobsProcess$center, oper = "+")
colnames(originalXobs) <- colnames(artmodel$data$Xobs)
originalXobs <- cbind(ModelSite = artmodel$data$ModelSite, originalXobs)

fit_runjags <- run.detectionoccupancy(originalXocc, cbind(originalXobs, artmodel$data$y), 
                                      species = colnames(artmodel$data$y),
                                      ModelSite = "ModelSite",
                                      OccFmla = artmodel$XoccProcess$fmla,
                                      ObsFmla = artmodel$XobsProcess$fmla,
                                      # initsfunction = function(chain, indata){return(NULL)},
                                      # MCMCparams = list(n.chains = 2 - as.numeric(x$nlv > 0), adapt = 1000, burnin = 10000, sample = 500, thin = 40),
                                      nlv = 4)
save(fit_runjags, artmodel, originalXocc, originalXobs, file = "./tests/testthat/benchmark_varietysitesmodel.Rdata")

gwk <- tibble::enframe(geweke.diag(fit_runjags, frac1=0.1, frac2=0.5)$z, name = "varname")
qqnorm(gwk$value)
qqline(gwk$value)
varname2type <- function(varnames){
  types <- case_when(
    grepl("lv.coef", varnames) ~ "LV Load",
    grepl("LV.*,1\\]", varnames) ~ "LV1",
    grepl("LV.*,2\\]", varnames) ~ "LV2",
    grepl("LV.*,3\\]", varnames) ~ "LV3",
    grepl("LV.*,4\\]", varnames) ~ "LV4",
    grepl("^(mu|tau)", varnames) ~ "Comm Param", #parameters of community distributions
    grepl("^u.b", varnames) ~ "Occu Coef",
    grepl("^v.b", varnames) ~ "Detn Coef",
    TRUE ~ "other"
  )
  return(types)
}
gwk %>%
  dplyr::mutate(type = varname2type(varname)) %>%
  dplyr::group_by(type) %>%
  dplyr::summarise(swp = shapiro.test(value)$p.value) %>%
  ggplot2::ggplot() +
  facet_wrap(~type) +
  geom_vline(aes(xintercept = 0.01)) +
  geom_point(aes(y = 0, x = swp)) + 
  scale_x_continuous(name = "Shapiro-Wilk p-value",
                     trans = "identity") +
  ggtitle("Geweke Convergence Statistics Normality Tests",
          subtitle = "0.01 threshold shown")
# LVs did not converge!
hist(fit_runjags$summaries[,"AC.300"]) #Autocorellation looks ok.


test_that("Posterior credible distribution overlaps true parameters", {
  var2compare <- colnames(artmodel$mcmc[[1]])
  inCI <- (fit_runjags$summaries[var2compare, "Lower95"] <= artmodel$mcmc[[1]][1, var2compare]) &
  (fit_runjags$summaries[var2compare, "Upper95"] >= artmodel$mcmc[[1]][1, var2compare])
  expect_gt(mean(inCI), 0.95)
})

test_that("Median of Posterior is close to true", {
  var2compare <- colnames(artmodel$mcmc[[1]])
  res <- artmodel$mcmc[[1]][1, var2compare] - fit_runjags$summaries[var2compare, "Median"] 
  expect_equivalent(res, rep(0, length(res)), tol = 0.1 * artmodel$mcmc[[1]][1, var2compare])
})

test_that("Fitted likelihood matches true likelihood", {
  cl <- parallel::makeCluster(20)
  lkl_runjags <- likelihoods.fit(fit_runjags, cl = cl)
  lkl_artmodel <- likelihoods.fit(artmodel, cl = cl)
  parallel::stopCluster(cl)
  expect_equivalent(Rfast::colmeans(lkl_runjags), Rfast::colmeans(lkl_artmodel), tol = 0.01)
  expect_equivalent(Rfast::colmeans(lkl_runjags) - Rfast::colmeans(lkl_artmodel),
                    rep(0, ncol(lkl_artmodel)),
                        tol = 0.1 * Rfast::colmeans(lkl_artmodel))
})

test_that("Expected Number of Detected Species", {
  cl <- parallel::makeCluster(10)
  Enumspec <- predsumspecies(fit_runjags, UseFittedLV = TRUE, cl = cl)
  Enumspec_art <- predsumspecies(artmodel, UseFittedLV = TRUE, cl = cl)
  parallel::stopCluster(cl)
  cbind(rj = t(Enumspec), art = t(Enumspec_art)) %>%
    as_tibble() %>%
    tibble::rowid_to_column() %>%
    ggplot()
  expect_equivalent(Enumspec, Enumspec_art)
  
  NumSpecies <- detectednumspec(y = fit_runjags$data$y, ModelSite = fit_runjags$data$ModelSite)
  
  meandiff <- dplyr::cummean(NumSpecies - Enumspec["Esum_det", ])
  meanvar <- cumsum(Enumspec["Vsum_det", ])/((1:ncol(Enumspec))^2)
  plt <- cbind(diff = meandiff, var  = meanvar) %>% 
    dplyr::as_tibble(rownames = "CumSites") %>% 
    dplyr::mutate(CumSites = as.double(CumSites)) %>%
    ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(x= CumSites, ymin = -2 * sqrt(var), ymax = 2 * sqrt(var)), fill = "grey") +
    ggplot2::geom_line(ggplot2::aes(x = CumSites, y = diff), col = "blue", lwd = 2)
  # print(plt)
  
  # check 'average' model site numbers are correct within standard errors, and that standard error is close.
  Enum_compare_sum <- Enum_compare(NumSpecies,
                                   as.matrix(Enumspec["Esum_det", ], ncol = 1),
                                   as.matrix(Enumspec["Vsum_det", ], ncol = 1)
  )
  expect_equivalent(Enum_compare_sum[["E[D]_obs"]], 0, tol = 3 * Enum_compare_sum[["SE(E[D]_obs)_model"]])
  expect_equivalent(Enum_compare_sum[["E[D]_obs"]], 0, tol = 3 * Enum_compare_sum[["SE(E[D]_obs)_obs"]])
  expect_equivalent(Enum_compare_sum[["V[D]_model"]], Enum_compare_sum[["V[D]_obs"]], tol = 0.05 * Enum_compare_sum[["V[D]_obs"]])
})

