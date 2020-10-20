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
birds_clean <- birds_cropped %>%
  dplyr::filter(CommonName %in% c(in7_2_10$species, "Noisy Miner"))
############## End Cleaning ###################
stopifnot(setequal(siteinfo$SiteCode, birds_clean$SiteCode))

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
stopifnot(setequal(siteinfo$SiteCode, birds_wide$SiteCode))

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
stopifnot(setequal(siteinfo$SiteCode, birds_wide$SiteCode))

#### Process so that each row and corresponds to one visit to a site (multiple plots), and any distance less than 50m ####
plotsmerged <- birds_wide %>%
  group_by(SurveyYear, SurveySiteId, SiteCode, RepeatNumber, SurveyDate) %>% #surveydate included here just in case, weird that the repeats have the same date
  summarise_at(.vars = vars(matches(CommonNames)), function(x) as.numeric(sum(x, na.rm = TRUE) > 0)) #detection simplified to binary per transect
stopifnot(setequal(siteinfo$SiteCode, plotsmerged$SiteCode))


## filter visits that have unequal effort
plotsmerged <- birds_wide %>%
  group_by(SurveyYear, SurveySiteId, SiteCode, RepeatNumber, SurveyDate) %>%
  summarise(plotamt = n(), maxplotnum = max(PlotNumber)) %>%
  inner_join(plotsmerged, by = c("SurveyYear", "SurveySiteId", "SiteCode", "RepeatNumber", "SurveyDate")) %>%
  dplyr::filter(plotamt == 3) %>% dplyr::select(-plotamt)
stopifnot(setequal(siteinfo$SiteCode, c("NMCG-2", "BARR-1", "BARR-4", "WOOD-1", plotsmerged$SiteCode)))
setdiff(siteinfo$SiteCode, plotsmerged$SiteCode) # sites have unusual plot amounts
birds_wide %>%
  dplyr::filter(SiteCode %in% setdiff(siteinfo$SiteCode, plotsmerged$SiteCode)) %>%
  group_by(SurveyYear, SurveySiteId, SiteCode, RepeatNumber, SurveyDate) %>%
  summarise(plotamt = n(), maxplotnum = max(PlotNumber))

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
stopifnot(setequal(siteinfo$SiteCode, c("NMCG-2", "BARR-1", "BARR-4", "WOOD-1", simplifiedcovars$SiteCode)))

plotsmerged <- inner_join(simplifiedcovars, plotsmerged) %>%
  ungroup()
stopifnot(setequal(siteinfo$SiteCode, c("NMCG-2", "BARR-1", "BARR-4", "WOOD-1", plotsmerged$SiteCode)))

##### Remove PlotMerged-Visits with NA values for required 7_4 information  ####
plotsmerged <- plotsmerged %>%
  dplyr::filter(across(any_of(c("SiteCode", "SurveyYear",
                                "MeanTime",
                                names(NameMap),
                                "Noisy Miner")
                                ), ~ !is.na(.x)))
stopifnot(setequal(siteinfo$SiteCode, c("NMCG-2", "BARR-1", "BARR-4", "WOOD-1", plotsmerged$SiteCode)))

##### Convert Noisy Miners into a Site-Level covariate ####
plotsmerged_detection <- plotsmerged %>% dplyr::select(-`Noisy Miner`)   #use Noisy Miner like an environmental covariate

stopifnot(any(CommonNames %in% colnames(plotsmerged_detection)))