#### On Ground Environment Observations  ####
# the following tests that detection information hasn't been lost for the sites
stopifnot(length(setdiff(siteinfo$SiteCode, sites_onground$SiteCode)) == 71) # 71 sites have no biometrics visits

predictorstokeep <- setdiff(c("% Native overstory cover", "% Native midstory cover", colnames(in7_2_10$insampledata$Xocc)),
                      c("SiteCode", "ModelSiteID", "SurveySiteId", "SurveyYear", "StudyId", "holdout"))

# keep only columns of interest
sites_onground <- sites_onground %>%
  dplyr::select(StudyId, SiteCode, SurveySiteId, SurveyYear, any_of(predictorstokeep))

# four sites x SurveyYear have NA native midstorey
sites_onground %>%
  dplyr::filter(is.na(SiteCode) | is.na(SurveyYear) | is.na(`% Native midstory cover`))

# get average values for each site
sites_onground_av <- sites_onground %>%
  group_by(SiteCode, SurveySiteId) %>%
  dplyr::summarise(across(any_of(predictorstokeep), ~ mean(.x, na.rm = TRUE)))

# data frame of whether noisy miners were detected at each site, for each year, in any of the bird visits
NoisyMinerDetected_BirdSurvey <- plotsmerged %>%
  group_by(SurveySiteId, SiteCode, SurveyYear) %>%
  summarise(NMdetected = max(`Noisy Miner`))
# 68 biometric vists have occured on years that weren't visited by birds (or at least not recorded)
right_join(NoisyMinerDetected_BirdSurvey, sites_onground, by = c("SiteCode", "SurveySiteId", "SurveyYear")) %>%
  dplyr::select(SiteCode, SurveyYear, NMdetected) %>%
  dplyr::filter(is.na(NMdetected)) %>%
  dplyr::mutate(SiteCode = as.factor(SiteCode)) %>%
  summary()

occ_covariates <- sites_onground_av %>%
  dplyr::filter(SurveySiteId %in% plotsmerged_detection$SurveySiteId) %>% #remove the sites that are not present in the detection data (useful when I'm testing on subsets)
  inner_join(NoisyMinerDetected_BirdSurvey, by = c("SurveySiteId", "SiteCode")) #each row is a unique combination of site and survey year
# sum(!(plotsmerged_detection$SiteCode %in% occ_covariates$SiteCode))
# length(unique(plotsmerged_detection$SiteCode[!(plotsmerged_detection$SiteCode %in% occ_covariates$SiteCode)]))

#### Remote and Climate Observations ####
locs <- read.csv("./private/data/raw/all_lindenmayer_sites_wgs84coords.csv", check.names = FALSE)[, -1] %>%
  dplyr::filter(SiteCode %in% plotsmerged_detection$SiteCode)
locs$MeanLON <- rowMeans(locs[, c("LON0m", "LON100m", "LON200m")], na.rm = TRUE)
locs$MeanLAT <- rowMeans(locs[, c("LAT0m", "LAT100m", "LAT200m")], na.rm = TRUE)
locs <- sf::st_as_sf(locs, coords = c("MeanLON", "MeanLAT"), crs = 4326)
# raster::rasterOptions(tmpdir = "/media/kassel/Seagate1TB/tmpdir/")
# woodyclim <- sustfarmld::ll2webdata(locs, 2000:2019)
# woodyclim <- cbind(locs, woodyclim)
# save(woodyclim, file = "./tmpdata/woodyclim.RData")
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
  full_join(occ_covariates, by = c("SiteCode", "Year" = "SurveyYear")) %>%
  rename(SurveyYear = Year)

# add climate info
occ_covariates <- left_join(occ_covariates, as_tibble(woodyclim) %>% dplyr::select(-starts_with("w500m")), by = "SiteCode")
