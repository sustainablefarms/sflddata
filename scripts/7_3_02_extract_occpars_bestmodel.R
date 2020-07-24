fit <- readRDS("./tmpdata/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO.rds")
XoccProcess <- fit$XoccProcess
theta <- get_theta(fit, type = "median")
u.b <- bugsvar2matrix(theta, "u.b", 1:fit$data$n, 1:ncol(fit$data$Xocc))
rownames(u.b) <- fit$species
colnames(u.b) <- colnames(fit$data$Xocc)
saveRDS(list(XoccProcess = XoccProcess, 
             u.b = u.b), "./private/models/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO_extract.rds")

draws <- do.call(rbind, fit$mcmc)
draws_sub <- draws[, grepl("^u.b", colnames(draws))]
saveRDS(draws_sub, file = "./private/models/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO_draws.rds")
