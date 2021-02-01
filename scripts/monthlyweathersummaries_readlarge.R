datadir <- "~/LargeWorkData/AnnualWeatherSummaries/"


library(dplyr)
# prepare site info
siteLUtable <- readxl::read_excel(paste0(datadir, "Sit_numeric_ID_and_name_lookup_table_2020.xlsx"))
stopifnot(sum(!complete.cases(siteLUtable)) == 0)


siteinfo_bggw <- readRDS(paste0("./private/data/", "raw/sites_basic_boxgum.rds")) %>% dplyr::select(-DateTimeModified)
siteinfo_other <- readRDS(paste0("./private/data/", "raw/sites_basic_other.rds")) %>% dplyr::select(-DateTimeModified)
siteinfo <- bind_rows(siteinfo_bggw, siteinfo_other)
colnames(siteinfo)

unique(siteLUtable$Study)
unique(siteinfo$StudyCode)
tostudycode <- c(Stewardship = "Stewardship Project",
  Nanangroe = "Nanangroe Natural Experiment",
  SWS = "Restoration Study",
  BBMP = "BBMP")
siteLUtable$StudyCode <- tostudycode[siteLUtable$Study]
unique(siteLUtable$StudyCode)
siteLUtable$Study <- NULL
  


# filter only climate info for wanted sites
climvar <- read.table(paste0(datadir, "AllClimate.csv"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
stopifnot(sum(!complete.cases(climvar)) == 0)


climvarsite <- dplyr::full_join(climvar, siteLUtable, by = c(Site = "SiteID")) %>% dplyr::select(-Site)
climvar_cut <- dplyr::semi_join(climvarsite, siteinfo, by = c("StudyCode", "SiteCode"))
site_missing <- dplyr::anti_join(siteinfo, climvarsite, by = c("StudyCode", "SiteCode")) #should be none, or very few
stopifnot(nrow(site_missing) == 0)

# simplify data - at this point it is 19MB saved in compressed form
##Evap: number between 0 and 350. Wonder what it's units are
#Frost: between 0 and 31, guessing a days with frost estimate. Very heavily skewed towards zero.
#Rain T* make sense
# Srad restricted to between 5 and 31 ish? Average doesn't look like it would be a good summary. solar radiation (?kJ m-2 day-1)
# Vp and Vpd appear to positive, but not sure what it is either. Avereage could be an ok summary. Vapour pressue (?kPa)
# want for a 12 month period finishing before spring the worldclim bioclim variables. Computable using dismo::bioclim (https://www.worldclim.org/data/bioclim.html).
# Plus Frost, Srad, Vp, Vpd, Evap:  will need to form annual summaries
annual_sum <- climvar_cut %>%
  dplyr::select(-Longitude, -Latitude) %>% 
  dplyr::mutate(YearFinishAug = case_when(
    Month <= 8 ~ Year,
    Month > 8 ~ Year + as.integer(1))) %>%
  dplyr::group_by(StudyCode, SiteCode, YearFinishAug) %>%
  summarise(bio = dismo::biovars(prec = Rain, tmin = Tmin, tmax = Tmax),
            Frost = sum(Frost),
            Srad = mean(Srad),
            Vp = mean(Vp),
            Vpd = mean(Vpd)) %>%
  dplyr::ungroup()

annual_sum <- cbind(annual_sum %>% dplyr::select(-bio), data.frame(annual_sum[, "bio", drop = TRUE]))

# correct names
colnames(annual_sum) <- gsub("^X", "bio", colnames(annual_sum))
climate_names_table <- readRDS("./inst/climate_names_table.rds")
code2shortname <- climate_names_table[, c("code", "shortname") ] %>% tibble::deframe()
colnames(annual_sum)[colnames(annual_sum) %in% names(code2shortname)] <-
  code2shortname[colnames(annual_sum)[colnames(annual_sum) %in% names(code2shortname)]]

# save data
saveRDS(annual_sum %>% dplyr::filter(YearFinishAug >= 1997), file = "./private/data/raw/annualised_climvar_bggw_other_sites.rds")
  


# explore data
# climvar_cut %>%
#   dplyr::filter(SiteCode == "GRAB-C") %>%
#   summary()
# 
# annual_sum %>%
#   as.data.frame() %>% # removes the subcolumns of bio
#   tidyr::pivot_longer(c(-SiteCode, -YearFinishAug)) %>%
#   ggplot() +
#   geom_histogram(aes(x = value)) +
#   facet_wrap(vars(name), scales = "free")
# annual_sum <- climvar_cut %>%

# look at long term summary to see how close it is to the other worldclim data
longtermsummary <- 
  climvar_cut %>%
  dplyr::select(-Longitude, -Latitude) %>% 
  dplyr::group_by(StudyCode, SiteCode) %>%
  summarise(bio = dismo::biovars(prec = Rain, tmin = Tmin, tmax = Tmax),
            Frost = sum(Frost),
            Srad = mean(Srad),
            Vp = mean(Vp),
            Vpd = mean(Vpd)) %>%
  dplyr::ungroup()
hist(longtermsummary$bio[, 5]) #this looks more reasonable than what I'm seeing from raster::getData (raster version 3.0-7)
hist(longtermsummary$bio[, 6])
