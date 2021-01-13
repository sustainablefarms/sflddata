# Comparing the point-transect data to the older style records
biomveg <- readRDS("./private/data/raw/sws_veg_biom_raw.rds")
plantingsitedata <- readRDS("./private/data/raw/sws_planting_siteveg_raw.rds")
plantingplotdata <- readRDS("./private/data/raw/sws_planting_plotveg_raw.rds")

# veg things for midstorey
vegMSvarnames <- biomveg %>%
  dplyr::filter(SurveyType == "veg") %>%
  dplyr::filter(grepl("MS", varname)) %>%
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
  arrange(mssum, numdim) %>%
  View()
# nearly every site adds up to 100. To me this means that the values are the proportion of midstorey that is of each type. 
# Rather than the area of midstorey cover relative to the ground area.

# No other variable names in the site data appear to fit.
# This information appears to be in the plot data (strange!)
plantingplotdata %>%
  distinct(Variable) %>%
  unlist() %>%
  as.vector()

plantingplotdata %>%
  group_by(SiteCode, SurveyDate) %>%
  dplyr::filter(Variable == "MS(=2m, <10m)%Cover") %>%
  summarise(nplots = n_distinct(PlotNumber),
            MSCover = mean(Value, na.rm = TRUE)) %>%
  summary()
