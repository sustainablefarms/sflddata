# Extract TWI
# data described here: https://www.clw.csiro.au/aclep/soilandlandscapegrid/ProductDetails-LandscapeAttributes.html
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate);library(dplyr);
library(slga) #specialist library for reading the data
source("./R/sites_2_sp_points.R")

# get spatial points
source("./R/sites_2_sp_points.R")
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_sf(sws_sites)

# TWI at points
get_lscape_point(slga_product_info %>%
                   filter(Product == "Topographic Wetness Index") %>%
                   select(Short_Name) %>%
                   as.character(),
                 c(148.0779, -35.13167))
get_lscape_point(slga_product_info %>%
                   filter(Product == "Topographic Wetness Index") %>%
                   select(Short_Name) %>%
                   as.character(),
                   points[1, ])

# TWI for whole region
twi_ras <- get_lscape_data(slga_product_info %>%
                   filter(Product == "Topographic Wetness Index") %>%
                   select(Short_Name) %>%
                   as.character(),
                   aoi = points)

#twi at the points
twi_pts <- extract(twi_ras, points)
