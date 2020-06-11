# extracting percent treecover. Starts as 25m, but has been resampled to 250m GSD
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./R/", list.files("./R/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
roi <- extent(buffer(points, 1000)) #the buffer here to make sure extracted brick includes extra around the points

files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/treecover",
                    "250m",
                    "ANUWALD.TreeCover",
                    c(2000, 2002, 2004:2017), #2001, 2003 is missing
                    "250m",
                    "nc")

b <- extract_brick_files(files, "TreeCover", roi,
                    dims = c(2, 1, 3))
names(b) <- c(2000, 2002, 2004:2017)

#compute average of buffer for every pixel
wf <- focalWeight(b, 0.005, type = "circle") #0.005 corresponds to 2x resolution which is 250m
bs <- focal_bylayer(b, wf, fun = sum)
bs_newproj <- projectRaster(bs, brick("./private/data/derived/m1b_resid_Sept6th.grd"),  method = "bilinear")
stopifnot(all(minValue(bs_newproj) >= 0))
writeRaster(bs_newproj, "./private/data/remote_sensed/treecover_all_500mrad.grd", overwrite = TRUE)


treecover_500mradius <- t(extract(bs, points))
colnames(treecover_500mradius) <- points$SiteCode
years <- year(as_date(rownames(treecover_500mradius), format =  "X%Y", tz = "Australia/Sydney"))
treecover_500mradius <- cbind(year = years, data.frame(treecover_500mradius))
session <- sessionInfo()


# Checking against old version
# l <- new.env()
# load("./private/data/remote_sensed/treecover_500mradius.Rdata", envir = l)
# l$treecover_500mradius
# 
# library(ggplot2)
# site <- sym(sample(points$SiteCode, 1))
# ggplot() +
#   geom_line(aes(year, !! site), data = treecover_500mradius) +
#   geom_line(aes(year, !! site), data = l$treecover_500mradius, col = "red")
  

save(treecover_500mradius, session, file = "./private/data/remote_sensed/treecover_500mradius.Rdata")