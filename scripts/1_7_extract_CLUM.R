# Catchment scale land use and management (CLUM)
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./R/", list.files("./R/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- spTransform(sws_sites_2_spdf(sws_sites),
                      CRS("+init=epsg:3577"))  #this is the crs that raster extracts from the .tif
roi <- extent(buffer(points, 1000)) #the buffer here to make sure extracted brick includes extra around the points

rtemp <- raster("//fses52-003/MyDocuments$/hingeek/My Documents/AustLandUseCatchmentScale_2018version/geotiff_clum_50m1218m_AustLandUse/geotiff_clum_50m1218m/clum_50m1218m_with_colour_ramp.tif")
classesdf <- rtemp@data@attributes[[1]]



r <- raster("//fses52-003/MyDocuments$/hingeek/My Documents/AustLandUseCatchmentScale_2018version/geotiff_clum_50m1218m_AustLandUse/geotiff_clum_50m1218m/clum_50m1218m.tif")
rc <- crop(r, roi, snap = "out")

plot(rc)
plot(add = TRUE, points)


# Extract values 
siteCLUM <- extract(rc, points)
names(siteCLUM) <- points$SiteCode
siteCLUM <- data.frame(CLUM_ID = siteCLUM, SiteCode = names(siteCLUM))
siteCLUM <- merge(siteCLUM, classesdf, by.x = "CLUM_ID", by.y = "ID", sort = FALSE)
row.names(siteCLUM) <- siteCLUM$SiteCode
siteCLUM <- siteCLUM[points$SiteCode, ]
# Would be really good to get historical land use (e.g. 2000s, years of the SWS)