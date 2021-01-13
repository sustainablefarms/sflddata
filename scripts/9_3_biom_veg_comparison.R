# Comparing the point-transect data to the older style records
biomveg <- readRDS("./private/data/raw/sws_veg_biom_raw.rds")
plantingsitedata <- readRDS("./private/data/raw/sws_planting_siteveg_raw.rds")
plantingplotdata <- readRDS("./private/data/raw/sws_planting_plotveg_raw.rds")

# midstorey amounts in veg data
biomveg %>%
  dplyr::filter(varname == "MS(=2m, <10m)%Cover") %>%
  distinct(SiteCode) %>%
  summary()
length(unique(biomveg$SiteCode)) 
biomveg %>%
  dplyr::filter(varname == "MS(=2m, <10m)%Cover") %>%
  summary()
# 24 sites are missing the MS(=2m, <10m)%Cover value. Are these the sites that also have biometrics though?

# the biometric midstorey data is in the column NtvMSCover, except the number appears to vary between rows for a single site.
# I wonder how that is possible. I'll take the average anyway.
biomveg %>%
  dplyr::filter(SurveyType == "biom") %>%
  group_by(SiteCode, SurveyDate) %>%
  summarise(value = sd(value, na.rm = TRUE),
            CanopySpace = sd(CanopySpace, na.rm = TRUE),
            ExtcWoodyCover = sd(ExtcWoodyCover, na.rm = TRUE),
            NtvOSCover = sd(NtvOSCover, na.rm = TRUE),
            NtvMSCover = sd(NtvMSCover, na.rm = TRUE)) %>%
  summary()

# taking average now
biom_extra <- biomveg %>%
  dplyr::filter(SurveyType == "biom") %>%
  group_by(SurveyType, SiteCode, GrowthType, `Keith's Class`, YearPlanted, SurveyYear, SurveyDate) %>%
  summarise(CanopySpace = mean(CanopySpace, na.rm = TRUE),
            ExtcWoodyCover = mean(ExtcWoodyCover, na.rm = TRUE),
            NtvOSCover = mean(NtvOSCover, na.rm = TRUE),
            NtvMSCover = mean(NtvMSCover, na.rm = TRUE)) %>%
  tidyr::pivot_longer(c(CanopySpace, ExtcWoodyCover, NtvOSCover, NtvMSCover),
                      names_to = "varname",
                      values_to = "value") %>%
  ungroup()

biomveg2 <- bind_rows(biomveg, biom_extra) %>%
  dplyr::select(!c(CanopySpace, ExtcWoodyCover, NtvOSCover, NtvMSCover))

biomveg2 %>%
  dplyr::filter(varname == "NtvMSCover") %>%
  summary()

# compare the two distributions
biomveg2 %>%
  dplyr::filter(varname %in% c("NtvMSCover", "MS(=2m, <10m)%Cover")) %>%
  ggplot() +
  geom_histogram(aes(x = value, y = ..density..)) +
  facet_grid(cols  = vars(GrowthType), rows = vars(varname))

# the shapes are a bit different when looking at all SWS sites.

# Restrict to just survey days that both occured
bothsitedates <-  biomveg2 %>%
  dplyr::filter(varname %in% c("NtvMSCover", "MS(=2m, <10m)%Cover")) %>%
  group_by(SiteCode, SurveyDate) %>%
  summarise(n = n()) %>%
  dplyr::filter(n == 2) %>%
  dplyr::select(SiteCode, SurveyDate) %>%
  ungroup()
  
biomveg2 %>%
  inner_join(bothsitedates) %>%
  dplyr::filter(varname %in% c("NtvMSCover", "MS(=2m, <10m)%Cover")) %>%
  ggplot() +
  geom_point(aes(y = SiteCode, x = value, col = varname, shape = varname)) +
  facet_grid(rows  = vars(GrowthType), space = "free_y", scales = "free_y")
# In this figure the NtvMSCover is consistently lower than the MS(=2m, <10m)%Cover value. This is consistent with what David has told me.

intersectiondata <- biomveg2 %>%
  inner_join(bothsitedates) %>%
  dplyr::filter(varname %in% c("NtvMSCover", "MS(=2m, <10m)%Cover")) %>%
  dplyr::select(!SurveyType) %>%
  tidyr::pivot_wider(names_from = varname, values_from = value) %>%
  dplyr::filter(!is.na(NtvMSCover), !is.na(`MS(=2m, <10m)%Cover`)) %>%
  rename(MSveg = `MS(=2m, <10m)%Cover`,
         MSbiom = NtvMSCover)

intersectiondata %>%
  ggplot() +
  geom_point(aes(x = MSveg, y = MSbiom, col = GrowthType, shape = GrowthType),
             position = "jitter") +
  # facet_grid(rows  = vars(GrowthType), space = "free_y", scales = "free_y") +
  scale_x_continuous()

intersectiondata %>%
  dplyr::filter(MSveg> 0) %>%
  ggplot() +
  geom_point(aes(x = MSveg, y = MSbiom, col = GrowthType, shape = GrowthType),
             position = position_jitter(width = 1, height = 0.2)) +
  coord_cartesian(xlim = c(-2, 20), ylim = c(-1, 5))
  scale_x_continuous()
# association seems to be dominated by MSbiom being zero
# for example at about MSveg = 8 and MSveg = 15, there is a > 50% chance that MSbiom will be zero.

fit <- lm(MSbiom ~ MSveg, intersectiondata)
summary(fit)
plot(fit) #linear model is definitely *not* appropriate

##### CONCLUSION ####
# It will be hard to map MSveg to MSbiom. Often (more than 50% of the time) MSbiom is zero even for high values of MSveg.
# This suggests it can't be used in a model with both plantings and remnants.

## Appendix
# veg components of midstorey
vegMSvarnames <- biomveg %>%
  dplyr::filter(SurveyType == "veg") %>%
  dplyr::filter(grepl("%MS", varname)) %>%
  distinct(varname) %>%
  unlist() %>%
  as.vector()

biomveg %>%
  dplyr::select(!c(CanopySpace, ExtcWoodyCover, NtvOSCover, NtvMSCover)) %>%
  dplyr::filter(SurveyType == "veg", varname %in% vegMSvarnames) %>%
  group_by(varname) %>%
  summarise(n = n_distinct(SiteCode, SurveyDate)) %>%
  arrange(n)

biomveg %>%
  dplyr::select(!c(CanopySpace, ExtcWoodyCover, NtvOSCover, NtvMSCover)) %>%
  dplyr::filter(SurveyType == "veg", varname %in% vegMSvarnames) %>%
  ggplot() +
  geom_point(aes(y = SiteCode, x = value, col = varname)) +
  facet_grid(cols = vars(varname), rows = vars(GrowthType))
# the above two computations show that midstorey variables have differing numbers of recordings, perhaps the missing ones are zero

biomveg %>%
  dplyr::filter(SurveyType == "veg", varname %in% vegMSvarnames) %>%
  group_by(SiteCode, SurveyDate) %>%
  summarise(mssum = sum(value),
            numdim = n()) %>%
  summary()
# nearly every site adds up to 100. To me this means that the values are the proportion of midstorey that is of each type. 
# Rather than the area of midstorey cover relative to the ground area.
