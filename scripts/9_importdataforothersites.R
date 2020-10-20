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

# compare to site information
siteinfo <- readRDS("./private/data/raw/sites_basic_other.rds")
stopifnot(setequal(siteinfo$SiteCode, birds_raw$SiteCode))

# for getting covariates relevant to the 7_4 model
in7_2_10 <- readRDS("./private/data/clean/7_2_10_input_data.rds")

# on ground measurements
sites_onground <- readRDS("./private/data/raw/othersite_covar_grnd.rds")

# clean the detection data
source("./scripts/9_1_clean_detectiondata.R")

#### Save the SurveyYear and SiteCodes used - these will become the ModelSites ####
ModelSites <- plotsmerged_detection %>%
  dplyr::select(SiteCode, SurveyYear) %>%
  dplyr::distinct()

#### Occupancy Data Preparation ####
source("./scripts/9_2_clean_occupancydata.R")

stopifnot(setequal(siteinfo$SiteCode, c("NMCG-2", "BARR-1", "BARR-4", "WOOD-1", occ_covariates$SiteCode))) 

#### Contextual Information ####
occ_covariates <- left_join(occ_covariates, siteinfo[, c("SiteCode", "VegType", "Context")], by = "SiteCode")

#### Polish data sets ####
# rename covariates for convenience
occ_covariates <- occ_covariates %>% 
  rename(os = "% Native overstory cover",
         ms = "% Native midstory cover") %>%
  mutate(gc = `Exotic sub-shrub` + `Native sub-shrub` + 
           Cryptograms + `Native forbs/herbs/other` + `Organic litter` + `Exotic broadleaf/forb/other` +
           + `Coarse woody debris`)

# use intersection of sites in occ_covariates and plotsmerged_detection
occ_covariates <- occ_covariates %>%
  inner_join(ModelSites, by = c("SiteCode", "SurveyYear"))

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


