# reading PCT data test
library(rgdal)
library(raster)
library(sf)

source("./R/sites_2_sp_points.R")
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_sf(sws_sites)

extent(points)
# ogr2ogr -clipsrc 146.6404 -35.93073 148.1461 -34.64599  -f "ESRI Shapefile" SVTM_Riverina_v1p2B.gdb   SVTM_Riverina_v1p2.gdb
# Above use of ogr2ogr created errors. It might be that the best way to use the map is via a rasterisation


gdb <- "../LargeInputData/vegetationsvtmriverinav1.2quickviews4469/SVTM_Riverina_v1p2.gdb"
gdb <- "/media/kassel/KassTraveler/vegetationsvtmriverinav1.2quickviews4469/SVTM_Riverina_v1p2.gdb"
layers <- st_layers(gdb)

IDtable <- st_read(gdb, layer = "PCT_Structural_Riverina_LUT_v1p2") #table of PCT IDs and corresponding names, Keith class and Keith form

PCTsurveysites <- st_transform(st_read(gdb, layer = "Riverina_v1p2_SurveySites"), st_crs(points))

boundary <- st_as_sf(readOGR(gdb, layer = "Riverina_v1p2_Boundary"))
boundary <- st_transform(boundary, st_crs(points))

sheetboundary <- st_as_sf(readOGR(gdb, layer = "Riverina_100k_Sheet_Boundaries"))
sheetboundary <- st_transform(sheetboundary, st_crs(points))

unknown <- st_transform(st_read(gdb, layer = "SVTM_Riverina_v1p2_5m"), st_crs(points)) #what is this?

quickview <- st_transform(st_read(gdb, layer = "Riverina_v1p2_Quickview"), st_crs(points))


boundary %>%
  ggplot() +
  geom_sf() +
  geom_sf(data = quickview) +
  geom_sf(data = sheetboundary, fill = NA) +
  geom_sf(data = PCTsurveysites, col = "green", pch = "+") +
  geom_sf(data = points)
