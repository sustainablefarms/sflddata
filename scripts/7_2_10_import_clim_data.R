library(tidyr)
library(dplyr)

#### Load Data ####

data_7_2_8 <- readRDS("./private/data/clean/7_2_8_input_data.rds")
Xocc <- rbind(data_7_2_8$insampledata$Xocc, data_7_2_8$holdoutdata$Xocc)
yXobs <- rbind(data_7_2_8$insampledata$yXobs, data_7_2_8$holdoutdata$yXobs)
species <- data_7_2_8$species
climdata <- read.csv("./private/data/clean/worldclim_data.csv", stringsAsFactors = FALSE)
names(climdata) <- gsub("_410", "", names(climdata))

#### Better Climate Data Names ####
# From descriptions on https://worldclim.org/data/bioclim.html (visited July 1, 2020)
climshortname <- c(
  bio1 = "AnnMeanTemp",
  bio2 = "DiurnalRange",
  bio3 = "Isothermality",
  bio4 = "TempSeasonality",
  bio5 = "MaxTWarmMonth",
  bio6 = "MinTColdMonth",
  bio7 = "AnnTempRange",
  bio8 = "MnTWetQ",
  bio9 = "MnTDryQ",
  bio10 = "MnTWarmQ",
  bio11 = "MnTColdQ",
  bio12 = "AnnPrec",
  bio13 = "PrecWetMonth",
  bio14 = "PrecDryMonth",
  bio15 = "PrecSeasonality",
  bio16 = "PrecWetQ",
  bio17 = "PrecDryQ",
  bio18 = "PrecWarmQ",
  bio19 = "PrecColdQ"
)
climdescription = c(
  bio1 = "Annual Mean Temperature",
  bio2 = "Mean Diurnal Range (Mean of monthly (max temp - min temp))",
  bio3 = "Isothermality (BIO2/BIO7) (×100)",
  bio4 = "Temperature Seasonality (standard deviation ×100)",
  bio5 = "Max Temperature of Warmest Month",
  bio6 = "Min Temperature of Coldest Month",
  bio7 = "Temperature Annual Range (BIO5-BIO6)",
  bio8 = "Mean Temperature of Wettest Quarter",
  bio9 = "Mean Temperature of Driest Quarter",
  bio10 = "Mean Temperature of Warmest Quarter",
  bio11 = "Mean Temperature of Coldest Quarter",
  bio12 = "Annual Precipitation",
  bio13 = "Precipitation of Wettest Month",
  bio14 = "Precipitation of Driest Month",
  bio15 = "Precipitation Seasonality (Coefficient of Variation)",
  bio16 = "Precipitation of Wettest Quarter",
  bio17 = "Precipitation of Driest Quarter",
  bio18 = "Precipitation of Warmest Quarter",
  bio19 = "Precipitation of Coldest Quarter"
)
climnamesdf <- data.frame(code = names(climshortname), shortname = climshortname, description = climdescription[names(climshortname)])
saveRDS(climnamesdf, file = "./private/data/clean/7_2_10_climate_names_table.rds")

names(climdata) <-
  c("SiteCode", "longitude", "latitude",
    climnamesdf[names(climdata)[c(-1, -2, -3)], "shortname"])


##### merge in climate data ####
Xocc <- left_join(Xocc, climdata, by = c("SiteCode"))

#### No NA values to clean out ####
stopifnot(!anyNA(Xocc))

#### Correct ModelSiteID to have holdout sites first ####
Xocc <- Xocc %>% arrange(holdout)
Xocc <- Xocc %>% dplyr::select(-ModelSiteID) %>% tibble::rowid_to_column(var = "ModelSiteID")
yXobs <- Xocc[ , c("ModelSiteID", "SurveySiteId", "SurveyYear")] %>%
  inner_join(yXobs %>% dplyr::select(-ModelSiteID) , by = c("SurveySiteId", "SurveyYear"))

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
        file = "./private/data/clean/7_2_10_input_data.rds")

