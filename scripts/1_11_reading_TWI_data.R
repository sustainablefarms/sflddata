# Extract TWI
# data described here: https://www.clw.csiro.au/aclep/soilandlandscapegrid/ProductDetails-LandscapeAttributes.html
library(slga) #specialist library for reading the data
library(sf)
locs <- read.csv("private/data/clean/site_locations_cleaned.csv", stringsAsFactors = FALSE)
locs_wgs84 <- st_as_sf(locs, coords = c("MeanLON", "MeanLAT"))
locs_wgs84 <- st_set_crs(locs_wgs84, 4326) #4326 is the epsg code for WGS84, make this default crs I'll return to

twiprodcode <- slga_product_info %>%
  filter(Product == "Topographic Wetness Index") %>%
  select(Short_Name) %>%
  as.character()

twis <- lapply(1:nrow(locs_wgs84), function(id) {
  get_lscape_point(twiprodcode,
                     poi = locs_wgs84[id, ])
  })
twis <- unlist(twis)
twidf <- data.frame(TWI = twis, SiteCode = locs_wgs84$SiteCode)
saveRDS(twidf, "./private/data/remote_sensed/TWI_boxgum_sites.rds")


# TWI for whole region
# twi_ras <- get_lscape_data(slga_product_info %>%
#                    filter(Product == "Topographic Wetness Index") %>%
#                    select(Short_Name) %>%
#                    as.character(),
#                    aoi = locs_wgs84[locs_wgs84$StudyCode == "Nanangroe Natural Experiment", ])
# plot(twi_ras)
# plot(locs_wgs84, add = TRUE)
