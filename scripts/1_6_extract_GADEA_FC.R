# Fractional Cover Version 2.0
# information on the data is here: http://dapds00.nci.org.au/thredds/catalog/fk4/datacube/002/FC/catalog.html  Readme is useful (confirms belief about tile format)
# 


invisible(lapply(c("raster", "maptools", "rgdal", "ncdf4", "lubridate"),
                 library, character.only = TRUE))
invisible(lapply(paste0("../linking-data/functions/", list.files("../linking-data/functions/")), source))


# Construct Region Desired
sws_sites <- readRDS("../linking-data/data/clean/sws_sites.rds")
ptsraw <- sws_sites_2_spdf(sws_sites)
points <- spTransform(sws_sites_2_spdf(sws_sites),
                      CRS("+init=epsg:3577"))
roi <- extent(buffer(points, 1000)) #the buffer here to make sure extracted brick includes extra around the points


#tile codes:
tilestep <- 100000
lxmin <- floor(roi@xmin / tilestep) * tilestep #lowest xmin
xmins <- seq(lxmin, -1 + ceiling(roi@xmax / tilestep) * tilestep,
             by = tilestep)
lymin <- floor(roi@ymin / tilestep) * tilestep #lowest ymin
ymins <- seq(lymin, -1 + ceiling(roi@ymax / tilestep) * tilestep,
             by = tilestep)

xmin_v_ymin <- expand.grid(xmin = xmins, ymin = ymins)
tilecodes <- apply(xmin_v_ymin / tilestep, 1, function(x) paste(x, collapse = "_"))
names(tilecodes) <- tilecodes

tilecode <- tilecodes[[2]]

files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/fk4/datacube/002/FC/FC-percentile/ANNUAL",
                    tilecode[[1]],
                    paste0("LS_FC_PC_3577_", tilecode, "_"),
                    2000:2018,
                    "0101.nc",
                    namesep = "")
nc <- nc_open(files[[1]])

# I think the values are statistics for each year. BS_PC_50 *I think* is the median percent of bare soil for a pixel in the given year. I think the data file I've chosen to open above is created as a demo here: https://docs.dea.ga.gov.au/notebooks/09_Workflows/Fractional_Cover_Percentiles.html