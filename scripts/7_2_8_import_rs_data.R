library(readxl)
library(tidyr)
library(dplyr)

#### Load Data ####

data_7_2_7 <- readRDS("./private/data/clean/7_2_4_input_data.rds")
Xocc <- rbind(data_7_2_7$insampledata$Xocc, data_7_2_7$holdoutdata$Xocc)
yXobs_grnd <- rbind(data_7_2_7$insampledata$yXobs, data_7_2_7$holdoutdata$yXobs)
species <- data_7_2_7$species

gpp <- readRDS("./private/data/remote_sensed/8d_gpp.rds")
woody <- readRDS("./private/data/remote_sensed/woody500.rds")
names(woody) <- gsub("^X", "", names(woody))
twi <- readRDS("./private/data/remote_sensed/TWI_boxgum_sites.rds")

#### Impute Woody Data ####
woody$`2019` <- woody$`2018` #impute the 2019 woody veg values

#### Compute GPP Data #####
SiteCodeColIdx <- which(names(gpp) == "SiteCode")
gpp_means <- rowMeans(gpp[, -SiteCodeColIdx])
gpp_means <- data.frame(GPPmean = gpp_means, SiteCode = gpp$SiteCode)

# difference to means
gpp_diff <- gpp[, -SiteCodeColIdx] - gpp_means$GPPmean  #high gpp_diff means higher than mean GPP values
gpp_diff$SiteCode <- gpp$SiteCode
stopifnot(isTRUE(all.equal(gpp_diff[, 5], gpp[, 5] - gpp_means$GPPmean)))

# interpolate to days in between for matching to visit dates
datename_2_date <- function(x) {
  posixltval <- lubridate::fast_strptime(x, format = "X%Y.%m.%d", tz = "Australia/Sydney")
  out <- as.POSIXct(posixltval)
  return(out)
}
obsdates <- datename_2_date(names(gpp_diff)[-SiteCodeColIdx]) + hours(12)   #the addition of 12 avoids funky things with daylight saving and missing a date
xout <- seq(min(obsdates), max(obsdates), by = '1 day') 
gpp_diff_interp <- apply(gpp_diff[, -SiteCodeColIdx], MARGIN = 1, 
                           FUN = function(x) {
                             interp <- approx(obsdates, x, xout = xout)
                             return(interp$y)}
)
gpp_diff_interp <- as.data.frame(t(gpp_diff_interp))
colnames(gpp_diff_interp) <- format(xout, "%Y-%m-%d")
gpp_diff_interp$SiteCode <- gpp_diff$SiteCode

# extract mean of these on SurveyVisit days for the GPP of a model site
gpp_diff_modelsite <- gpp_diff_interp %>%
  pivot_longer(-SiteCode, names_to = "Date", values_to = "GPPdiff") %>%
  right_join(yXobs_grnd[, c("SurveyDate", "SurveyYear", "SiteCode")], by = c("Date" = "SurveyDate", "SiteCode")) %>%
  group_by(SurveyYear, SiteCode) %>%
  summarise(meanGPPdiff = mean(`GPPdiff`), .groups = "drop") 

#### Add Remote Sensing Data to Available Covariates ####
# first have to map to join in SiteCode
sites.boxgum <- readRDS("./private/data/raw/sites_basic_boxgum.rds")
stopifnot(anyDuplicated(sites.boxgum$SurveySiteID) == 0)
stopifnot(anyDuplicated(sites.boxgum$SiteCode) == 0)
SurveySiteId2SiteCodeMap <- deframe(sites.boxgum[, c("SurveySiteID", "SiteCode")])

# add sitecode
Xocc$SiteCode <- SurveySiteId2SiteCodeMap[as.character(Xocc$SurveySiteId)]

# add TWI
twi <- deframe(twi[, c(2, 1)])
Xocc$TWI <- twi[ Xocc$SiteCode ]

# add GPP mean
gpp_means_map <- deframe(gpp_means[, c(2, 1)])
Xocc$GPPmean <- gpp_means_map[ Xocc$SiteCode ]

# add GPP diff
Xocc <- left_join(Xocc, gpp_diff_modelsite, by = c("SiteCode", "SurveyYear"))
Xocc <- Xocc %>% rename(GPPdiff = meanGPPdiff)

# add woody veg
Xocc <- woody %>%
  pivot_longer(-SiteCode, names_to = "Year", values_to = "woody500m") %>%
  mutate(Year = as.integer(Year)) %>%
  right_join(Xocc, by = c("SiteCode", "Year" = "SurveyYear")) %>%
  rename(SurveyYear = Year)

#### Clean out NA values (only 1998 visits) ####
# Xocc %>%
#   filter_all(any_vars(is.na(.))) 
Xocc <- Xocc %>%
  filter_all(all_vars(!is.na(.)))

#### Reorder Columns for Convenience
newcolorder <- c("ModelSiteID", "SiteCode", "SurveySiteId", "SurveyYear", "StudyId",
  "woody500m", "TWI", "GPPmean", "GPPdiff",
  "Native perenial grass", "Native sub-shrub", "Native forbs/herbs/other",
  "Exotic perenial grass", "Exotic sub-shrub", "Exotic annual grass",  "Exotic broadleaf/forb/other",
  "Bare ground", "Rock", "Organic litter", "Cryptograms", "Coarse woody debris",
  "os", "ms", "gc", "NMdetected", "holdout"
  )
stopifnot(setequal(newcolorder, names(Xocc)))
Xocc <- Xocc[, newcolorder]

#### Recompute ModelSite ####
Xocc <- Xocc %>% dplyr::select(-ModelSiteID) %>% rowid_to_column(var = "ModelSiteID")
yXobs <- Xocc[ , c("ModelSiteID", "SurveySiteId", "SurveyYear")] %>%
  inner_join(yXobs_grnd %>% dplyr::select(-ModelSiteID) , by = c("SurveySiteId", "SurveyYear"))


#### Separate back into holdout data ####
insampledata <- list(
  Xocc = Xocc %>% dplyr::filter(!holdout),
  yXobs = yXobs %>% dplyr::filter(!holdout)
)

holdoutdata <- list(
  Xocc = Xocc %>% dplyr::filter(holdout),
  yXobs = yXobs %>% dplyr::filter(holdout)
)


# DBI::dbDisconnect(con)
saveRDS(list(insampledata = insampledata,
             holdoutdata = holdoutdata,
             species = species),
        file = "./private/data/clean/7_2_8_input_data.rds")

