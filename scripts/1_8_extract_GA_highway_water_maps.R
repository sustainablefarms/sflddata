# Small Subsets of Australia-wide Maps from GA
library(dplyr); library(sf)

## The Geoscience Australia GEODATA TOPO 250K Series 3 Data:
# Geoscience Australia (GA) supplies maps of roads, waterways and many other features for Australia.
# A list of the relevant data is [here](http://www.ga.gov.au/scientific-topics/national-location-information/topographic-maps-data/digital-topographic-data).
# Selecting GEODATA TOPO 250K Series 3 opens a new page, defaulting to esri Personal database. One of the 'related' links on the side is for the same data in shape file format.
# That webpage currently has address [https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/64058].
# This data, downloaded on 13 December 2019, is what is used for the script below.

## Projection Information
# According to metadata documents with this data, the projection is latitude and longitude, and the datum is GDA94.
# The datum GDA94 uses GRS80 as the ellipsoid ([Geocentric Datum of Australia 2020 Technical Manual](https://www.icsm.gov.au/sites/default/files/GDA2020TechnicalManualV1.1.1.pdf) available from [here](https://www.icsm.gov.au/datum/geocentric-datum-australia-1994-gda94). I'm not sure what other things the datum specifies beyond the ellipsoid.
# The CRS information extracted from the shape files agrees - they give a proj4string of "+proj=longlat +ellps=GRS80 +no_defs".

## Read the relevant shape files:
dir_of_vector_data <- "C:/UserData/hingeek/GA_TOPO_250K_Series_3_shp/Vector_data/"
railways <- st_read(paste0(dir_of_vector_data, "Transport/railways.shp"))
waterareas <- st_read(paste0(dir_of_vector_data, "Hydrography/watercourseareas.shp"))
waterlines <- st_read(paste0(dir_of_vector_data, "Hydrography/watercourselines.shp"))  #takes a minute or two
roads <- st_read(paste0(dir_of_vector_data, "Transport/roads.shp"))  #takes a minute or two
sf.l <- list(railways = railways, waterareas = waterareas, waterlines = waterlines, roads = roads)

# Extract the features more useful for wide area maps from above
sf.l[["waterlines"]] <- sf.l[["waterlines"]] %>% dplyr::filter(HIERARCHY == "Major") 
sf.l[["waterlines"]]$FEATTYPE <- recode(sf.l[["waterlines"]]$FEATTYPE,
                                        "Connector" = "Major Watercourse",
                                        "Watercourse" = "Major Watercourse")
#according to User Guide for GEODATA TOPO 250K Series 3 User Guide (2006) [p33] the water course connectors are used to connect across area featurs to allow for network analysis of rivers.
sf.l[["roads"]] <- sf.l[["roads"]] %>% dplyr::filter(CLASS == "Principal Road")
sf.l[["roads"]]$FEATTYPE <- recode(sf.l[["roads"]]$FEATTYPE, "Road" = "Principal Road")


# Combine features into single SF object
names_common <- Reduce(intersect, lapply(sf.l, names))
allsf <- Reduce(function(x, y) rbind(x[, names_common], y[, names_common]),
                                    sf.l)

saveRDS(allsf, file = "./private/data/GA_principalroads_majorrivers_railsways.rds")
# Crop objects to SE corner of Australia covering NSW with generous margins
# allsf_crop <- st_crop(allsf, c(xmin = 138.6007, # Adelaide
#                                ymin = -37.8136, # Melbourne
#                                xmax = 155, # > xmax of data
#                                ymax = -27.4698))  #Brisbane

# saveRDS(allsf_crop, file = "./private/data/GA_principalroads_majorrivers_railsways_NSW+.rds")
