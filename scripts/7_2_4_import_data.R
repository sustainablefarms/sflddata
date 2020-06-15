library(readxl)
library(tidyr)
library(dplyr)



#### Visit Information ########
# source("./private/data/raw/birds_sql.R")
birds_raw <- readRDS("./private/data/raw/birds_long.rds")
birds_raw$CommonName <- gsub("Horsfield\x92s Bushlark", "Horsfield's Bushlark", birds_raw$CommonName) #weird encoding of a bird name
birds_raw <- birds_raw %>% dplyr::filter(CommonName != "Corella sp.") #remove the single observation of Corella sp. as this could be at least two different common names.
# each visit is actually a visit of a particular plot for a particular site
# each row corrponds to a unique "SurveyVisitId", "SpeciesId", "DistanceId".

# source("./private/data/raw/visit_covar_data_sql.R")
visit_data <- readRDS("./private/data/raw/visit_covar_data.rds")
# the above has data for each visit: structural information like season, datatype, plotnumber and repeatnumber
# it also has covariates: date, starttime, season, Observer, wind, clouds, temperature
# **it should have SiteId too**
# each visit typically has 3 plots
# each visit to each plot is considered a different 'SurveyVisitId'
# abundance of each species noticed could be recorded in 'Abundance'?


#### Traits Information ####
traits <- read.csv("./private/data/raw/Australian_Bird_Data_Version_1.csv")
namedists <- adist(unique(birds_raw$CommonName), traits$X3_Taxon_common_name_2)
rownames(namedists) <- unique(birds_raw$CommonName)
colnames(namedists) <- traits$X3_Taxon_common_name_2
NameMap <- apply(namedists, 1, function(v){
  ind <- which.min(v)
  return(names(v)[[ind]])
})


#### Bird Cropping: Remove distances, seasons and species not interested in ####
birds_renamed <- birds_raw %>%
  mutate(CommonName = NameMap[CommonName])

birds_cropped <- birds_renamed %>%
  inner_join(visit_data, by = c("SurveyVisitId", "SurveySiteId")) %>%
# remove the Nil species
  dplyr::filter(CommonName != "Nil") %>%
# keep only visits in Season '1'
  dplyr::filter(SurveySeasonId == 1)

# first remove distances greater than 50
birds_cropped <- birds_cropped %>%
  dplyr::filter(DistanceId %in% 2:7)

## clean out birds based on taxon (see 0_3_compatiability_Garnett_bird_traits.Rmd)
excluded_orders <- c(
  "Caprimulgiformes", # Frogmouths
  "Accipitriformes", # Eagles, Kites, Goshawks and Osprey
  "Strigiformes", # Masked Owls
  "Falconiformes", # Falcons
  "Podicepiformes", # Greebes
  "Gruiformes", # Crakes, Rails and Swamphens
  "Pelecaniformes", # Herons, Egrets, Pelicans etc
  "Anseriformes" # Ducks, Geese
)

birds_to_exclude <- as_tibble(traits) %>%
  dplyr::filter((X11_Order_2 %in% excluded_orders) | (X3_Taxon_common_name_2 == "Australian Reed-Warbler")) %>%
  dplyr::distinct()

birds_clean <- birds_cropped %>%
  dplyr::filter(!(CommonName %in% birds_to_exclude$X3_Taxon_common_name_2))
############## End Cleaning ###################


############## Widen Table: 1 row per SurveyVisitId ######
CommonNames <- unique(birds_clean$CommonName)
birds_wide <- birds_clean %>%
  dplyr::group_by(SurveyVisitId, SurveySiteId, SiteCode, CommonName) %>%
  summarise(Abundance = sum(Abundance)) %>%
  pivot_wider(names_from = "CommonName",
              values_from = "Abundance",
              values_fill = list(Abundance = 0)) %>%
  ungroup()
stopifnot(anyDuplicated(birds_wide$SurveyVisitId) == 0)
stopifnot(anyDuplicated(visit_data$SurveyVisitId) == 0)

birds_wide <- birds_wide %>%
  inner_join(visit_data, by = c("SurveyVisitId", "SurveySiteId"))

#### Detection Covariates ####
## Parse Time Objects
library(lubridate)
birds_wide <- birds_wide %>%
  mutate(SurveyStartMinutesSinceMidnight = as.numeric(hms(SurveyStartTime)) / 60 )
sum(is.na(birds_wide$SurveyStartMinutesSinceMidnight)) / nrow(birds_wide) #6% of rows have no start time

# fix two outlying start times:
sitemeans <- birds_wide %>%
  dplyr::filter(SurveyStartMinutesSinceMidnight > 300) %>%
  group_by(SurveySiteId) %>%
  summarise(MeanVisitTime = mean(SurveyStartMinutesSinceMidnight)) %>%
  filter(SurveySiteId %in% c(1682, 1031)) %>%
  tibble::deframe()
birds_wide[birds_wide$RepeatNumber==1 & 
             birds_wide$SurveyYear==2016 & 
             birds_wide$SurveySiteId==1682, "SurveyStartMinutesSinceMidnight"] <- sitemeans["1682"]
birds_wide[birds_wide$RepeatNumber==2 & 
             birds_wide$SurveyYear==2011 & 
             birds_wide$SurveySiteId==1031, "SurveyStartMinutesSinceMidnight"] <- sitemeans["1031"]

## Clean Wind Information: replace 99 with NA, make ordered factor
birds_wide <- birds_wide %>%
  mutate(WindCode = factor(WindCode, levels = c("Calm", "Light", "Moderate", "Windy"),
                           exclude = "N/A",
                           labels = c("Calm", "Light", "Moderate", "Windy"),
                           ordered = TRUE))

#CloudsCode, remove raining because a different type of weather, and it is only 0.6% occurance!
birds_wide <-  birds_wide %>%
  mutate(Clouds = recode(CloudsCode,
                         "Clear" = 12.5, 
                         "Scattered" = 37.5, 
                         "Broken" = 62.5,
                         "Overcast" = 87.5,
                         "Raining" = as.numeric(NA),
                         "N/A" = as.numeric(NA)))


#Temperature Code
# corresponds to Id of 1, 2, 3, 4, 5, 99 in dbo schema
# corresponds to <15, Cool, 15-20, 20-30, 30+
## **What is 'Cool'!? I think cold is a typo - it can't be sub 15 (that is pretty comfortable temperature)
birds_wide <- birds_wide %>%
  mutate(Temperature = recode(TemperatureCode,
                              "Cold" = 2.5,
                              "Cool" = 10,
                              "Mild" = 17.5,
                              "Mild Warm" = 25,
                              "Warm" = 32.5,
                              "N/A" = as.numeric(NA)))

# Observer Id: do nothing!


#### Process so that each row and corresponds to one visit to a site (multiple plots), and any distance less than 50m ####
plotsmerged <- birds_wide %>%
  group_by(SurveyYear, SurveySiteId, SiteCode, RepeatNumber, SurveyDate) %>% #surveydate included here just in case, weird that the repeats have the same date
  summarise_at(.vars = vars(matches(CommonNames)), function(x) as.numeric(sum(x, na.rm = TRUE) > 0)) #detection simplified to binary per transect

## filter visits that have unequal effort
plotsmerged <- birds_wide %>%
  group_by(SurveyYear, SurveySiteId, SiteCode, RepeatNumber, SurveyDate) %>%
  summarise(plotamt = n(), maxplotnum = max(PlotNumber)) %>%
  inner_join(plotsmerged, by = c("SurveyYear", "SurveySiteId", "SiteCode", "RepeatNumber", "SurveyDate")) %>%
  dplyr::filter(plotamt == 3) %>% dplyr::select(-plotamt)

## summarise detection covariates
simplifiedcovars <- birds_wide %>%
  group_by(SurveyYear, SurveySiteId, SiteCode, RepeatNumber) %>%
  summarise(MeanWind = mean(as.numeric(WindCode), na.rm = TRUE),
            MeanTime = mean(SurveyStartMinutesSinceMidnight, na.rm = TRUE),
            MeanClouds = mean(Clouds, na.rm = TRUE),
            MeanTemp = mean(Temperature, na.rm = TRUE),
            ObserverId = first(ObserverId),
            NObservers = n_distinct(ObserverId)) %>% #summary for each transect
  ungroup()
stopifnot(all(simplifiedcovars$NObservers == 1))

plotsmerged <- inner_join(simplifiedcovars, plotsmerged) %>%
  ungroup()

##### Remove PlotMerged-Visits with NA values  ####
plotsmerged <- na.omit(plotsmerged)

############# remove birds that are rare #########
TotalAbundance <- plotsmerged %>%
  dplyr::select(tidyselect::any_of(CommonNames)) %>%
  colSums()
plotsmerged <- plotsmerged %>%
  dplyr::select(-names(TotalAbundance)[TotalAbundance <= 100])

##### Convert Noisy Miners into a Site-level covariate ####
plotsmerged_detection <- plotsmerged %>% dplyr::select(-`Noisy Miner`)   #use Noisy Miner like an environmental covariate

##### put species representing correlation clusters first (to help fix the LV sign) #####
# plotsmerged_detection <- plotsmerged_detection  %>%
#   dplyr::select(!any_of(CommonNames), `Superb Fairy-wren`, `Willie Wagtail`, everything())
species <- intersect(colnames(plotsmerged_detection), CommonNames)

#### On Ground Environment Observations  ####
# source("./private/data/raw/site_covar_data_sql.R")
sites_onground <- readRDS("./private/data/raw/site_covar_grnd.rds")
n_nNA <- function(x) {sum(!is.na(x))}
NumberOfMeasurements_study <- sites_onground %>%
  group_by(StudyId) %>%
  summarise_all(n_nNA) %>%
  t()
# Exotic woody cover is noticeably absent from Stewardship. 
# Study 2 is missing data on exotic woody cover.
# Restoration is missing data on exotic perenial grass, exotic annual grass, exotic broadleaf/forb/other, coarse woody debris, and canopy space
# Stewardship is missing data on exotic broadleaf/forb/other, Coarse woody debris, exotic woody cover and canopy space.

varstokeep <- setdiff(rownames(NumberOfMeasurements_study)[rowSums(NumberOfMeasurements_study) > 400], c("StudyId", "SurveySiteId"))
varstoremove <- setdiff(rownames(NumberOfMeasurements_study), c("StudyId", "SurveySiteId", varstokeep))

# clean out sites with NA values
sites_onground <- sites_onground %>%
  dplyr::select(StudyId, SurveySiteId, all_of(varstokeep)) %>%
  na.omit(sites_onground) #removes 79 of the 465 spatial locations, leaving 386.
# Removed locations due to missing native vegetation cover %s and Exotic broadleaf...

# ignoring Exotic broadleaf/forb/other would enable a 6% increase in spatial locations. MW's guess is that ground cover is not important.
# BUUUT from plot(as.data.frame(scale(sites_onground)))  it appears that Exotic broadleaf/forb/other is uncorrelated with any other data, so it could be useful.
# Most rigorous thing to do is include it, and then test if it is important.
# This is because the lost locations can be considered to be part of the same distribution as the rest of the locations,
# this is not the case for the Exotic broadleaf/forb/other data. 

# data frame of whether noisy miners were detected at each site, for each year, in any of the visits
NoisyMinerDetected <- plotsmerged %>%
  group_by(SurveySiteId, SurveyYear) %>%
  summarise(NMdetected = max(`Noisy Miner`))

occ_covariates <- sites_onground %>%
  dplyr::filter(SurveySiteId %in% plotsmerged_detection$SurveySiteId) %>% #remove the sites that are not present in the detection data (useful when I'm testing on subsets)
  inner_join(NoisyMinerDetected, by = "SurveySiteId") #each row is a unique combination of site and survey year

#### Mark Holdout Set All Data ####
holdout <- readRDS("./private/data/raw/sites_holdout.rds")

occ_covariates$holdout <- occ_covariates$SurveySiteId %in% holdout
plotsmerged_detection$holdout <- plotsmerged_detection$SurveySiteId %in% holdout

# arrange so holdout sites at end
occ_covariates <- occ_covariates %>% arrange(holdout)
plotsmerged_detection <- plotsmerged_detection %>% arrange(holdout)

#### Add a ModelSiteID ####
# has to be last so that the ModelSiteID value matches the rows in occ_covariates --> this is necessary for JAGS
occ_covariates <- occ_covariates %>% tibble::rowid_to_column(var = "ModelSiteID")  #site ID is a unique combination of site and survey year
plotsmerged_detection <- occ_covariates[ , c("ModelSiteID", "SurveySiteId", "SurveyYear")] %>%
  inner_join(plotsmerged_detection, by = c("SurveySiteId", "SurveyYear"))

insampledata <- list(
  Xocc = occ_covariates %>% dplyr::filter(!holdout),
  yXobs = plotsmerged_detection %>% dplyr::filter(!holdout)
)

holdoutdata <- list(
  Xocc = occ_covariates %>% dplyr::filter(holdout),
  yXobs = plotsmerged_detection %>% dplyr::filter(holdout)
)


# DBI::dbDisconnect(con)
saveRDS(list(insampledata = insampledata,
             holdoutdata = holdoutdata,
             species = species),
        file = "./private/data/clean/7_2_4_input_data.rds")


