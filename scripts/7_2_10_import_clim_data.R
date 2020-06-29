library(tidyr)
library(dplyr)

#### Load Data ####

data_7_2_8 <- readRDS("./private/data/clean/7_2_8_input_data.rds")
Xocc <- rbind(data_7_2_8$insampledata$Xocc, data_7_2_8$holdoutdata$Xocc)
yXobs <- rbind(data_7_2_8$insampledata$yXobs, data_7_2_8$holdoutdata$yXobs)
species <- data_7_2_8$species
climdata <- read.csv("./private/data/clean/worldclim_data.csv", stringsAsFactors = FALSE)
names(climdata) <- gsub("_410", "", names(climdata))

# merge in climate data
Xocc <- left_join(Xocc, climdata, by = c(SiteCode = "site_code"))

#### No NA values to clean out ####
stopifnot(!anyNA(Xocc))

#### ModelSite should be identical as no data removed ####

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

