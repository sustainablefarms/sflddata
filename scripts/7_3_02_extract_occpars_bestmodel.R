fit <- readRDS("./tmpdata/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO.rds")
XoccProcess <- fit$XoccProcess
u.b <- get_theta(fit, type = "median")
saveRDS(list(XoccProcess = XoccProcess, 
             u.b = u.b), "./private/models/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO_extract.rds")
