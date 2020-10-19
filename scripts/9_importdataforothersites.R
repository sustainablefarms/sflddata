library(readxl)
library(tidyr)
library(dplyr)



#### Visit Information ########
# source("./private/data/raw/birds_sql.R")
birds_raw <- readRDS("./private/data/raw/birds_othersites_long.rds")
birds_raw$CommonName <- gsub("Horsfield\x92s Bushlark", "Horsfield's Bushlark", birds_raw$CommonName) #weird encoding of a bird name
birds_raw <- birds_raw %>% dplyr::filter(CommonName != "Corella sp.") #remove the single observation of Corella sp. as this could be at least two different common names.
# each visit is actually a visit of a particular plot for a particular site
# each row corrponds to a unique "SurveyVisitId", "SpeciesId", "DistanceId".

# source("./private/data/raw/visit_covar_data_sql.R")
visit_data <- readRDS("./private/data/raw/other_visit_covar_data.rds")
# the above has data for each visit: structural information like season, datatype, plotnumber and repeatnumber
# it also has covariates: date, starttime, season, Observer, wind, clouds, temperature
# **it should have SiteId too**
# each visit typically has 3 plots
# each visit to each plot is considered a different 'SurveyVisitId'
# abundance of each species noticed could be recorded in 'Abundance'?


#### Bird Cropping: Remove distances, seasons and species not interested in ####
NameMap <- readRDS("./private/data/raw/7_2_4_birdNameMap.rds")
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

## keep only birds used in the 7_2_10 etc model
in7_2_10 <- readRDS("./private/data/clean/7_2_10_input_data.rds")


birds_clean <- birds_cropped %>%
  dplyr::filter(CommonName %in% c(in7_2_10$species, "Noisy Miner"))
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
sum(is.na(birds_wide$SurveyStartMinutesSinceMidnight)) / nrow(birds_wide) #7% of rows have no start time

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

#### birds_wide has many missing SurveyDate, SurveyYear is ok BUUT Survey Time is missing for these ####
birds_wide %>%
  dplyr::filter(is.na(SurveyDate)) %>%
  dplyr::select(-any_of(names(NameMap)), -WindCode, -CloudsCode, -TemperatureCode) %>%
  dplyr::mutate(SiteCode = as.factor(SiteCode),
                SurveyDate = as.factor(SurveyDate),
                SurveyStartTime = as.factor(SurveyStartTime)) %>%
  summary()

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

##### Remove PlotMerged-Visits with NA values for required 7_4 information  ####
plotsmerged <- plotsmerged %>%
  dplyr::filter(across(any_of(c("SiteCode", "SurveyYear",
                                "MeanTime",
                                names(NameMap),
                                "Noisy Miner")
                                ), ~ !is.na(.x)))

##### Convert Noisy Miners into a Site-Level covariate ####
plotsmerged_detection <- plotsmerged %>% dplyr::select(-`Noisy Miner`)   #use Noisy Miner like an environmental covariate

stopifnot(any(CommonNames %in% colnames(plotsmerged_detection)))

#### Save the SurveyYear and SiteCodes used - these will become the ModelSites ####
ModelSites <- plotsmerged_detection %>%
  dplyr::select(SiteCode, SurveyYear) %>%
  dplyr::distinct()

#### On Ground Environment Observations  ####
# source("./private/data/raw/site_covar_data_sql.R")
sites_onground <- readRDS("./private/data/raw/othersite_covar_grnd.rds")

predictorstokeep <- setdiff(c("% Native overstory cover", "% Native midstory cover", colnames(in7_2_10$insampledata$Xocc)),
                      c("SiteCode", "ModelSiteID", "SurveySiteId", "SurveyYear", "StudyId", "holdout"))

# keep only columns of interest
sites_onground <- sites_onground %>%
  dplyr::select(StudyId, SiteCode, SurveySiteId, SurveyYear, any_of(predictorstokeep))

# plot plantings
siteinfo <- readRDS("./private/data/raw/sites_basic_other.rds")
left_join(sites_onground, siteinfo, by = "SiteCode") %>%
  dplyr::filter(VegType %in% "Poss_SWS_Plantings")
siteinfo$SiteCode[siteinfo$VegType == "Poss_SWS_Plantings"] %in% sites_onground$SiteCode
stop("All the planting sites are not present in sites_onground!! :(")

# four sites x SurveyYear have NA native midstorey
sites_onground %>%
  dplyr::filter(is.na(SiteCode) | is.na(SurveyYear) | is.na(`% Native midstory cover`))

# get average values for each site
sites_onground_av <- sites_onground %>%
  group_by(SiteCode) %>%
  dplyr::mutate(across(any_of(predictorstokeep), ~ mean(.x, na.rm = TRUE), .names = "{.col}_av"))

# interpolate on-ground biometrics to bird survey years
interp <- function(a, ynames = predictorstokeep){
  for (yname in ynames){
    if (!(yname %in% colnames(a))){warning(paste(yname, "not in data")); next}
    if (sum(is.finite(a[, yname, drop = TRUE])) >= 2){ #if not enough points then do no interpolation
      a[, yname] <- approx(a$SurveyYear, a[, yname, drop = TRUE], xout = a$SurveyYear)$y
    }
  }
  return(a)
}

sites_onground_interp <- sites_onground %>%
  group_by(SiteCode) %>%
  full_join(ModelSites, by = c("SiteCode", "SurveyYear")) %>%
  dplyr::group_by(SiteCode) %>%
  group_modify(~ interp(.x, ynames = intersect(predictorstokeep, colnames(sites_onground)))) %>%
  right_join(ModelSites, by = c("SiteCode", "SurveyYear"))
  


# interpolate on-ground biometrics to bird survey years
library(ggplot2)
sites_onground %>%
  ggplot() +
  geom_point(aes(y = `% Native midstory cover`, x = SurveyYear, col = SiteCode))




# data frame of whether noisy miners were detected at each site, for each year, in any of the bird visits
NoisyMinerDetected_BirdSurvey <- plotsmerged %>%
  group_by(SurveySiteId, SurveyYear) %>%
  summarise(NMdetected = max(`Noisy Miner`))
# 68 biometric vists have occured on years that weren't visited by birds (or at least not recorded)
right_join(NoisyMinerDetected_BirdSurvey, sites_onground, by = c("SurveySiteId", "SurveyYear")) %>%
  dplyr::select(SiteCode, SurveyYear, NMdetected) %>%
  dplyr::filter(is.na(NMdetected)) %>%
  dplyr::mutate(SiteCode = as.factor(SiteCode)) %>%
  summary()


occ_covariates <- sites_onground %>%
  dplyr::filter(SurveySiteId %in% plotsmerged_detection$SurveySiteId) %>% #remove the sites that are not present in the detection data (useful when I'm testing on subsets)
  inner_join(NoisyMinerDetected, by = c("SurveySiteId", "SurveyYear")) #each row is a unique combination of site and survey year

#### Remote and Climate Observations ####
locs <- read.csv("./private/data/raw/all_lindenmayer_sites_wgs84coords.csv", check.names = FALSE)[, -1] %>%
  dplyr::filter(SiteCode %in% plotsmerged_detection$SiteCode, SiteCode %in% occ_covariates$SiteCode)
locs$MeanLON <- rowMeans(locs[, c("LON0m", "LON100m", "LON200m")], na.rm = TRUE)
locs$MeanLAT <- rowMeans(locs[, c("LAT0m", "LAT100m", "LAT200m")], na.rm = TRUE)
locs <- sf::st_as_sf(locs, coords = c("MeanLON", "MeanLAT"), crs = 4326)
raster::rasterOptions(tmpdir = "/media/kassel/Seagate1TB/tmpdir/")
woodyclim <- sustfarmld::ll2webdata(locs, 2000:2019)
woodyclim <- cbind(locs, woodyclim)
save(woodyclim, file = "./tmpdata/woodyclim.RData")
load("./tmpdata/woodyclim.RData")

# have lat lon explictly
woodyclim <- woodyclim %>%
  dplyr::mutate(longitude = sf::st_coordinates(geometry)[, 1],
                latitude = sf::st_coordinates(geometry)[, 2]) %>%
  as_tibble() %>%
  dplyr::select(-geometry)

# prep woody info differently to climate (as woody is per year)
woody <- woodyclim %>%
  as_tibble() %>%
  dplyr::select(SiteCode, starts_with("w500m"))
colnames(woody) <- gsub("^w500m", "", colnames(woody))
occ_covariates <- woody %>%
  pivot_longer(-SiteCode, names_to = "Year", values_to = "woody500m") %>%
  mutate(Year = as.integer(Year)) %>%
  right_join(occ_covariates, by = c("SiteCode", "Year" = "SurveyYear")) %>%
  rename(SurveyYear = Year)

# add climate info
occ_covariates <- left_join(occ_covariates, as_tibble(woodyclim) %>% dplyr::select(-starts_with("w500m")), by = "SiteCode")

#### Contextual Information ####
siteinfo <- readRDS("./private/data/raw/sites_basic_other.rds")
occ_covariates <- left_join(occ_covariates, siteinfo[, c("SiteCode", "VegType", "Context")], by = "SiteCode")

#### Polish data sets ####
# rename covariates for convenience
occ_covariates <- occ_covariates %>% 
  rename(os = "% Native overstory cover",
         ms = "% Native midstory cover") %>%
  mutate(gc = `Exotic sub-shrub` + `Native sub-shrub` + 
           Cryptograms + `Native forbs/herbs/other` + `Organic litter` + `Exotic broadleaf/forb/other` +
           + `Coarse woody debris`)

#### Add a ModelSiteID ####
# has to be last so that the ModelSiteID value matches the rows in occ_covariates --> this is necessary for JAGS
occ_covariates <- occ_covariates %>% tibble::rowid_to_column(var = "ModelSiteID")  #site ID is a unique combination of site and survey year
plotsmerged_detection <- occ_covariates[ , c("ModelSiteID", "SiteCode", "SurveySiteId", "SurveyYear")] %>%
  inner_join(plotsmerged_detection, by = c("SurveySiteId", "SurveyYear"))

data <- list(
  Xocc = occ_covariates,
  yXobs = plotsmerged_detection
)



saveRDS(data,
        file = "./private/data/clean/othersite_data.rds")


