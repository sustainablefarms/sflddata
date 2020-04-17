# Extract Soil Moisture Maps
knitr::opts_chunk$set(echo = TRUE)
out <- lapply(c("sf", "tsibble", 'lubridate', "viridis",
                'ggplot2', 'tidyr', 'grid', 'gridExtra', 
                'feasts', 'dplyr', 'gtable', 'fable',
                'mgcv', "raster", "sf", "ncdf4"),
              library, character.only = TRUE)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)


# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- spTransform(sws_sites_2_spdf(sws_sites),
                      CRS("+init=epsg:3577"))
roi <- buffer(points, 1000) #the buffer here to make sure extracted brick includes extra around the points


# Download Soil Moisture Data
ssoil_b <- brick_ssoil(roi, 2000:2019)
writeRaster(ssoil_b, "./tmpdata/ssoil_brick.grd", overwrite = TRUE)

# Following was used for development.
# nc <- nc_open("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/Ssoil/OzWALD.Ssoil.2006.nc")
# nc_close(nc)
# nc2 <- nc_open("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/daily/meteo/Pg/OzWALD.daily.Pg.2000.nc")
# nc_close(nc2)
