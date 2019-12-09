# Extract Woody Cover
# it is in Albers Tile format with CRS = CRS("+init=epsg:3577").
# (see analysis 1_2_checking_albers_tiles.Rmd for why)
# Tiles are in folders xmin_ymin in Easting and Northings, where xmin and ymin are in '000 000s of meters.

invisible(lapply(c("raster", "maptools", "rgdal", "ncdf4", "lubridate"),
       library, character.only = TRUE))
invisible(lapply(paste0("../linking-data/functions/", list.files("../linking-data/functions/")), source))


# Construct Region Desired
sws_sites <- readRDS("../linking-data/data/clean/sws_sites.rds")
ptsraw <- sws_sites_2_spdf(sws_sites)
points <- spTransform(sws_sites_2_spdf(sws_sites),
                      CRS("+init=epsg:3577"))
roi <- extent(points)


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


#build brick for each tile
brickfortile <- function(tilecode){
  filelist <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/LandCover/DEA_ALC",
                      tilecode,
                      paste0("fc_metrics_", tilecode, "_"),
                      2000:2018,
                      ".nc",
                      namesep = "")
  r.l <- lapply(filelist,
    function(x){
      tryCatch(
        {ras <- raster(x, varname = "WCF", dims = 2:1)
        return(ras)
        }
        ,
        warning = function(w) {
          if (!grep("cannot process these parts of the CRS", as.character(w))){
            warning(paste("For", x, w))
          } else {
            suppressWarnings(ras <- raster(x, varname = "WCF", dims = 2:1))
          }
        })
  })
  bs <- brick(r.l)
  bs <- crop(bs, roi)
  names(bs) <- 2000:2018
  return(bs)}
b.l <- lapply(tilecodes, brickfortile) 

# merge bricks
b <- Reduce(merge, b.l)
names(b) <- 2000:2018
proj4string(b) <- CRS("+init=epsg:3577")
writeRaster(b, "woodycover_brick.tif")


#extract time series for points with 500m buffer!
woodycover_500mradius <- t(extract(b,
                                  buffer(points, 500, dissolve = FALSE), 
                                  weights = TRUE,
                                  fun = mean))
colnames(woodycover_500mradius) <- points$SiteCode
years <- year(as_date(rownames(woodycover_500mradius), format =  "X%Y", tz = "Australia/Sydney"))
woodycover_500mradius <- cbind(year = years, data.frame(woodycover_500mradius))
session <- sessionInfo()
save(woodycover_500mradius, session, file = "./data/remote_sensed/woodycover_500mradius.Rdata")


