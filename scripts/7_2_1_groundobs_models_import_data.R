library(readxl)
library(tidyr)
library(dplyr)

# source("C:/Users/kassel/Documents/AccessLindenmayerSQL.R")

#### Visit Information ########
# source("./private/data/raw/birds_sql.R")
birds_raw <- readRDS("./private/data/raw/birds_long.rds")
# each visit is actually a visit of a particular plot for a particular site
# each row corrponds to a unique "SurveyVisitId", "SpeciesId", "DistanceId".
### anyDuplicated(birds_raw[, c("SurveyVisitId", "SpeciesId", "DistanceId")]) == 0
### sum(duplicated(birds_raw[, c("SurveyVisitId", "SpeciesId")])) > 0
# Taxon is all 'Bird'

# source("./private/data/raw/visit_covar_data_sql.R")
visit_data <- readRDS("./private/data/raw/visit_covar_data.rds")
# the above has daata for each visit: structural information like season, datatype, plotnumber and repeatnumber
# it also has covariates: date, starttime, season, Observer, wind, clouds, temperature
# **it should have SiteId too**
# each visit typically has 3 plots
# each visit to each plot is considered a different 'SurveyVisitId'
# abundance of each species noticed could be recorded in 'Abundance'?

#### Remove distances, seasons and species not interested in ####
birds_clean <- birds_raw %>%
  inner_join(visit_data, by = c("SurveyVisitId", "SurveySiteId")) %>%
# remove the Nil species
  dplyr::filter(CommonName != "Nil") %>%
# keep only visits in Season '1'
  dplyr::filter(SurveySeasonId == 1)

# first remove distances greater than 50
birds_clean <- birds_clean %>%
  dplyr::filter(DistanceId %in% 2:7)

## clean out water-focused birds,  birds that eat mostly vertbrates, and the Australian Reed Warbler
traits_ikin <- as.data.frame(
  readxl::read_excel(
    "./private/data/raw/Ikin_SWS_Bird_Traits_updatedApril2017.xlsx",
    sheet = "Ikin_SWS_Bird_Traits"
  ))
species_to_remove_ikin <- traits_ikin %>%
  filter((diet == "Vertebrates") |
           (substrate == "Water") |
           (species == "Australian Reed-Warbler")) %>%
  dplyr::select(species) %>%
  arrange(species) %>%
  unlist() %>%
  as.vector()
species_to_keep_ikin <- setdiff(unique(traits_ikin$species), species_to_remove_ikin)

# The only species that is here due to a typo is the Australian Reed Warbler
species_to_remove <- Conigrave::find_similar(species_to_remove_ikin, unique(birds_clean$CommonName), percent = 10)
stopifnot(length(species_to_remove) < length(species_to_remove_ikin)) #longer means too easy matching
species_to_keep <- Conigrave::find_similar(species_to_keep_ikin, unique(birds_clean$CommonName), percent = 10)
stopifnot(length(species_to_keep) < length(species_to_keep_ikin))  #longer means too easy matching

# Ikin traits are the most cannon. The Ikin trait table has no NA values.
# Are there additional species in my data to worry about?
birds_clean %>%
  dplyr::filter(!(CommonName %in% c(species_to_keep, species_to_remove)))  %>%
  group_by(CommonName) %>%
  summarise(Abundance = sum(Abundance)) %>%
  arrange(-Abundance)
# Yes there are birds that are not mentioned in traits_ikin, but they have very low abundance

# In the following find and remove birds that aren't in Ikin's traits and also largely subsist on vertebrates, or rely on water
# source("./private/data/raw/bird_traits_sql.R")
traits <- readRDS("./private/data/raw/bird_traits.rds")
species_to_remove_extra <- traits %>%
  dplyr::filter((grepl("Vertebrate.*", MainFood, ignore.case = FALSE, perl = TRUE)) |
                  (grepl("aquatic", MainFood, ignore.case = TRUE)) |
                  (grepl("water", ForagingMethod, ignore.case = TRUE)) |
                  (grepl("water", MainNestSite, ignore.case = TRUE)) |
                  (grepl("water", NestType, ignore.case = TRUE)) |
                  (CommonName == "Australian Reed Warbler")) %>%  #because there is a typo between the common names
  dplyr::select(CommonName) %>%
  dplyr::arrange(CommonName) %>%
  unlist() %>%
  as.vector() %>%
  setdiff(species_to_remove) %>%
  setdiff(species_to_keep)

#with all that process, now finally remove the species
species_to_remove_final <- c(species_to_remove, species_to_remove_extra)
birds_clean <- birds_clean %>%
  dplyr::filter(!(CommonName %in% species_to_remove_final))
# **What about migratory birds?!
############## End Cleaning ###################


############# remove birds that are rare #########
TotalAbundance <- birds_clean %>%
  group_by(CommonName) %>%
  summarise(TotalAbundance = sum(Abundance))
birds_clean <- birds_clean %>%
  dplyr::filter(CommonName %in% TotalAbundance$CommonName[TotalAbundance$TotalAbundance > 100])

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

##### Convert Noisy Miners into a Site-level covariate ####
plotsmerged_detection <- plotsmerged %>% dplyr::select(-`Noisy Miner`)   #use Noisy Miner like an environmental covariate

##### put species representing correlation clusters first (to help fix the LV sign) #####
plotsmerged_detection <- plotsmerged_detection  %>%
  dplyr::select(!any_of(CommonNames), `Superb Fairy-wren`, `Willie Wagtail`, everything())
detection_data_specieslist <- intersect(colnames(plotsmerged_detection), CommonNames)

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

#### Latest Possible: Remove Holdout Set from All Data ####
holdout <- readRDS("./private/data/raw/sites_holdout.rds")

occ_covariates <- occ_covariates %>%
  dplyr::filter(!(SurveySiteId %in% holdout))
plotsmerged_detection <- plotsmerged_detection %>%
  dplyr::filter(!(SurveySiteId %in% holdout))

#### Sanity check that holdout sites span all studies (which they did on May 18, 2020):
sites_onground %>% 
  dplyr::filter(SurveySiteId %in% holdout) %>%
  group_by(StudyId) %>%
  summarise(NumHeldoutSites = n_distinct(SurveySiteId))

#### Add a ModelSiteID ####
# has to be last so that the ModelSiteID value matches the rows in occ_covariates --> this is necessary for JAGS
occ_covariates <- occ_covariates %>% tibble::rowid_to_column(var = "ModelSiteID")  #site ID is a unique combination of site and survey year
plotsmerged_detection <- occ_covariates[ , c("ModelSiteID", "SurveySiteId", "SurveyYear")] %>%
  inner_join(plotsmerged_detection, by = c("SurveySiteId", "SurveyYear"))

# DBI::dbDisconnect(con)



