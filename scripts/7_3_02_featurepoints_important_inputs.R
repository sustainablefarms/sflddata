indata <- readRDS("./private/data/clean/7_2_10_input_data.rds")

##### Midstorey #####
hist(indata$insampledata$Xocc$ms, breaks = 40)
ms_pts <- quantile(indata$insampledata$Xocc$ms, prob = c(0, 0.8, 0.9))
saveRDS(ms_pts, file = "./private/data/derived/ms_features.rds")
# names(ms_pts) <- c("nearly none", "tiny", "some")

##### Woody Veg ####
hist(indata$insampledata$Xocc$woody500m, breaks = 40)
woody_pts <- quantile(indata$insampledata$Xocc$woody500m, prob = c(0.1, 0.5, 0.9))
saveRDS(woody_pts, file = "./private/data/derived/woody500m_features.rds")
# names(woody_pts) <- c("Very little", "some", "lots")