context("Remote Sensing Data Extraction")
library(testthat); library(maptools); library(raster); library(ncdf4)

test_that("Annual precipitation extracted at ARCH-1 properly (which is extraction of transposed dimensions data)", {
  source("./functions/sites2spatialpoints.R")
  source("./functions/extract_ts_wald.R")
  sws_sites <- readRDS("./data/sws_sites.rds")
  points <- swssites2spdf(sws_sites)[1, ]
  filelocation <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/annual/OzWALD.annual.Pg.AnnualSums.nc"
  varname = "AnnualSums"
  tseries <- extract_ts_wald(points,
         filelocation,
         varname = varname)
  refvals <- read.csv("./tests/testthat/-35.1320-148.0780_Precipitation_annual.csv") #downloaded from explorer linked to here: http://wald.anu.edu.au/australias-environment/
  expect_equivalent(tseries, refvals$Value, tol = 1E-4)
})

test_that("Annual minimum temperature ARCH-1 is extracted correctly (this file's dimensions aren't transposed)", {
  source("./functions/sites2spatialpoints.R")
  source("./functions/extract_ts_wald.R")
  sws_sites <- readRDS("./data/sws_sites.rds")
  points <- swssites2spdf(sws_sites)[1, ]
  filelocation <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/daily/meteo/Tmin/OzWALD.Tmin.2018.nc"
  varname = "Tmin"
  tseries <- extract_ts_wald(points,
         filelocation,
         varname = varname)
  refvals <- read.csv("./tests/testthat/-35.1320-148.0780_Minimum temperature_2018.csv") #downloaded from explorer linked to here: http://wald.anu.edu.au/australias-environment/
  expect_equivalent(tseries, refvals$Value, tol = 1E-4)
})

## Would love another test here of GPP data, but ncdf4 can't open GPP file: http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc