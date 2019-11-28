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

## Below GPP tests can't be run on my linux machine.

test_that("extract_ts_wald() matches GPP values extracted from the web explorer (dimensions transposed)", {
  source("./functions/sites2spatialpoints.R")
  source("./functions/extract_ts_wald.R")
  sws_sites <- readRDS("./data/sws_sites.rds")
  points <- swssites2spdf(sws_sites)[1, ]
  tseries <- extract_ts_wald(points,
                             "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc",
                             varname = "GPP")
  refvals <- read.csv("./tests/testthat/-35.1320-148.0780_Vegetation carbon uptake_2000.csv") #downloaded from explorer linked to here: http://wald.anu.edu.au/australias-environment/
  expect_equivalent(tseries, refvals$Value, tol = 1E-4)
})

test_that("extract_ts_wald() matches GPP values extracted by Marta (dimensions transposed)", {
  # Marta-Extracted FMC data 
  df <- read.table("./data/Birdsite_GPP_FMC_pointdrills_GPP.csv",
                   skip = 5, 
                   blank.lines.skip = TRUE, 
                   sep = c(",", "\n"),
                   header = TRUE)
  colnames(df)[1:3] <- c("year", "month", "day")
  colnames(df) <- sub("\\.", "-", colnames(df))
  longlats <- df[1:2, 5:ncol(df)]
  row.names(longlats) <- df[1:2, 4]
  gppvals <- df[4:nrow(df),-4]
  
  # Kass-Extracted FMC Data
  source("./functions/sites2spatialpoints.R")
  source("./functions/extract_ts_wald.R")
  sws_sites <- readRDS("./data/sws_sites.rds")
  points <- swssites2spdf(sws_sites)[1:10, ]
  tseries <- extract_ts_wald(points,
                             "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc",
                             varname = "GPP",
                             nl = 2) 
  
  expect_equivalent(t(tseries)[, "ARCH-1"], gppvals[1:2, "ARCH-1"])
  expect_equivalent(t(tseries)[, "ARCH-2"], gppvals[1:2, "ARCH-2"])
  expect_equivalent(t(tseries)[, "ARCH-3"], gppvals[1:2, "ARCH-3"])
})

test_that("extract_ts_wald() matches FMC values extracted by Marta (dimensions not transposed)", {
  # Marta-Extracted FMC data 
  df <- read.table("./data/Birdsite_GPP_FMC_pointdrills_FMC.csv",
                   skip = 5, 
                   blank.lines.skip = TRUE, 
                   sep = c(",", "\n"),
                   header = TRUE)
  colnames(df)[1:3] <- c("year", "month", "day")
  colnames(df) <- sub("\\.", "-", colnames(df))
  longlats <- df[1:2, 5:ncol(df)]
  row.names(longlats) <- df[1:2, 4]
  fmcvals <- df[4:nrow(df),-4]
  
  # Kass-Extracted FMC Data
  source("./functions/sites2spatialpoints.R")
  source("./functions/extract_ts_wald.R")
  sws_sites <- readRDS("./data/sws_sites.rds")
  points <- swssites2spdf(sws_sites)[1:2, ]
  tseries <- extract_ts_wald(points,
                             "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/FMC/c6/mosaics/fmc_c6_2001.nc",
                             varname = "fmc_mean",
                             nl = 2) 
  tseriesdates <- as.POSIXlt(as.numeric(gsub("X", "", colnames(tseries))), origin = lubridate::origin)
  colnames(tseries) <- as.character(tseriesdates)
  
  expect_equivalent(t(tseries)[, "ARCH-1"], fmcvals[1:2, "ARCH-1"])
  expect_equivalent(t(tseries)[, "ARCH-2"], fmcvals[1:2, "ARCH-2"])
  #t(tseries)
  #fmcvals[1:2, row.names(tseries)]
})
