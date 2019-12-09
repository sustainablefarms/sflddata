# extracting percent treecover. Starts as 25m, but has been resampled to 250m GSD
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

# Construct Region Desired
sws_sites <- readRDS("./data/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
roi <- extent(points)

files <- build_filename_list("http://dapds00.nci.org.au/thredds/dodsC/ub8/au/treecover",
                    "250m",
                    "ANUWALD.TreeCover",
                    c(2000, 2002, 2004:2017), #2001, 2003 is missing
                    "250m",
                    "nc")

b <- extract_brick_files(files, "TreeCover", roi,
                    dims = c(2, 1, 3))
names(b) <- c(2000, 2002, 2004:2017)

#percent tree cover in a 500m radius around point
treecover_500mradius <- t(extract(b,
                                buffer(points, 500, dissolve = FALSE), 
                                weights = TRUE,
                                fun = mean))
colnames(treecover_500mradius) <- points$SiteCode
years <- year(as_date(rownames(treecover_500mradius), format =  "X%Y", tz = "Australia/Sydney"))
treecover_500mradius <- cbind(year = years, data.frame(treecover_500mradius))
session <- sessionInfo()
save(treecover_500mradius, session, file = "./data/treecover_500mradius.Rdata")