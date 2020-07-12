# test that the following are consistent: 
# simulation data, runjags MCMC (posterior distribution matches true), predicted likelihoods, expected species detections, DS residuals

context("Wholistic tests using identical, independent, ModelSites")

# Create a process with known parameters
artmodel <- artificial_runjags(nspecies = 2, nsites = 1000, nvisitspersite = 2, nlv = 0,
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
  
  # likelihood by simulation
  sim_distr <- summary(factor(jointoutcomes))/length(jointoutcomes)
  lkl_sim <- sim_distr[jointoutcomes]
  
  # from this package
  lkl <- likelihoods.fit(fit_runjags)
  
  # compare 
  expect_equivalent(lkl, lkl_sim, tolerance = 0.05)
})

test_that("Expected Number of Detected Species", {
  Enumspec <- predsumspecies(fit_runjags, UseFittedLV = FALSE)
  NumSpecies <- detectednumspec(y = fit_runjags$data$y, ModelSite = fit_runjags$data$ModelSite)
  
  meandiff <- dplyr::cummean(NumSpecies - Enumspec["Esum_det", ])
  meanvar <- cumsum(Enumspecdet["Vsum_det", ])/((1:ncol(Enumspecdet))^2)
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
})



context("Wholistic tests on model with different ModelSites and LVs")

# Create a process with known parameters
artmodel <- artificial_runjags(nspecies = 2, nsites = 1000, nvisitspersite = 2, nlv = 4,
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

save(fit_runjags, artmodel, originalXocc, originalXobs, file = "./tests/testthat/benchmark_varietysitesmodel.Rdata")

test_that("Posterior credible distribution overlaps true parameters", {
  var2compare <- colnames(artmodel$mcmc[[1]])
  expect_true(all(fit_runjags$summaries[var2compare, "Lower95"] <= artmodel$mcmc[[1]][1, var2compare]))
  expect_true(all(fit_runjags$summaries[var2compare, "Upper95"] >= artmodel$mcmc[[1]][1, var2compare]))
})

test_that("Expected Number of Detected Species", {
  Enumspec <- predsumspecies(fit_runjags, UseFittedLV = FALSE)
  NumSpecies <- detectednumspec(y = fit_runjags$data$y, ModelSite = fit_runjags$data$ModelSite)
  
  meandiff <- dplyr::cummean(NumSpecies - Enumspec["Esum_det", ])
  meanvar <- cumsum(Enumspecdet["Vsum_det", ])/((1:ncol(Enumspecdet))^2)
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
})

