library(readxl)
library(tidyr)
library(dplyr)

birds_raw <- as.data.frame(
  read_excel(
    "./private/data/raw/LongTermStudies_BirdDataExtractions_19-03-2019.xlsx",
    sheet = "SWS"
  ))

#### Notes on this data
# for each survey vist (SurveyVisitId), no species is recorded twice
# each SiteCode has 1+ visits (called RepeatVisits 1, 2 ...)
# each visit typically has 3 plots
# each visit to each plot is considered a different 'SurveyVisitId'
# abundance of each species noticed could be recorded in 'SumAbundance'?

# reorganise into a visit (row) by species (col) matrix AND convert into binary of detection and not-detection
birds_wider <- birds_raw %>%
  mutate(Detected = as.integer(SumAbundance > 0)) %>%
  # filter(SurveyVisitId <= 13692) %>%
  dplyr::select(-ScientificName, -SumAbundance) %>%
  pivot_wider(names_from = "CommonName",
              values_from = Detected,
              values_fill = list(Detected = 0))

# birds_wider$SurveyVisitId should equal the unique SurveyVisitId
stopifnot(all.equal(unique(birds_raw$SurveyVisitId), birds_wider$SurveyVisitId))

species <- sort(unique(birds_raw$CommonName))

# remove the Nil species
birds_wider %>% 
  dplyr::select(-Nil)

# clean out water birds and birds that exclusively eat vertbrates
traits <- as.data.frame(
  read_excel(
    "./private/data/raw/Ikin_SWS_Bird_Traits_updatedApril2017.xlsx",
    sheet = "Ikin_SWS_Bird_Traits"
))

species_to_remove <- traits %>%
  filter((diet == "Vertebrates") |
           (substrate == "Water") |
           (species == "Australian Reed Warbler")) %>%
  dplyr::select(species) %>%
  unlist()

birds_clean <- birds_wider %>%
  dplyr::select(-species_to_remove)

# check!
stopifnot(setequal(species_to_remove, setdiff(colnames(birds_wider), colnames(birds_clean))))

# remove birds that are rare
detectnumber <- birds_clean %>%
  dplyr::select(matches(species)) %>%
  colSums()
birds_clean <- birds_clean %>%
  dplyr::select(-names(detectnumber)[detectnumber <= 100])

########################################################
## Parse Time Objects
library(lubridate)
birds_clean <- birds_clean %>%
  mutate(SurveyStartMinutesSinceMidnight = as.numeric(SurveyStartTime - lubridate::floor_date(SurveyStartTime, unit = "day"))/60)

## Clean Wind Information: replace 99 with NA
birds_clean <- birds_clean %>%
  mutate(WindId = replace(WindId, WindId == 99, NA))


########################################################
detection_data <- birds_clean
########################################################
