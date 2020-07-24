indata <- readRDS("./private/data/clean/7_2_10_input_data.rds")
fit <- readRDS("./tmpdata/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO.rds")

pbapply::pboptions(type = "none")
system.time({det_dist_2060 <- predsumspeciesRV_newdata(fit,
                                          chains = 1,
                                          Xocc = indata$holdoutdata$Xocc[indata$holdoutdata$Xocc$ModelSiteID == 2060, , drop = FALSE],
                                          ModelSiteVars = "ModelSiteID")})


system.time({sum_summ_2060 <- predsumspecies_newdata(fit,
                                                     chains = 1,
                                          Xocc = indata$holdoutdata$Xocc[indata$holdoutdata$Xocc$ModelSiteID == 2060, , drop = FALSE],
                                          ModelSiteVars = "ModelSiteID")})

