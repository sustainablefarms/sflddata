context("Remote Sensing Data Extraction")
skip()

test_that("Annual precipitation extracted at ARCH-1 properly (which is extraction of transposed dimensions data)", {
  skip_if_offline()
  sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
  points <- sws_sites_2_spdf(sws_sites)[1, ]
  filelocation <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/annual/OzWALD.annual.Pg.AnnualSums.nc"
  varname = "AnnualSums"
  tseries <- extract_ts_wald(points,
         filelocation,
         varname = varname)
  refvals <- read.csv("./tests/testthat/-35.1320-148.0780_Precipitation_annual.csv") #downloaded from explorer linked to here: http://wald.anu.edu.au/australias-environment/
  expect_equivalent(tseries[1:19], refvals$Value, tol = 1E-4)
})

test_that("Annual minimum temperature ARCH-1 is extracted correctly (this file's dimensions aren't transposed)", {
  skip_if_offline()
  sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
  points <- sws_sites_2_spdf(sws_sites)[1, ]
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
  skip_if_offline()
  sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
  points <- sws_sites_2_spdf(sws_sites)[1, ]
  tseries <- extract_ts_wald(points,
                             "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc",
                             varname = "GPP")
  refvals <- read.csv("./tests/testthat/-35.1320-148.0780_Vegetation carbon uptake_2000.csv") #downloaded from explorer linked to here: http://wald.anu.edu.au/australias-environment/
  expect_equivalent(tseries, refvals$Value, tol = 1E-4)
})

test_that("extract_ts_wald() matches GPP values extracted by Marta (dimensions transposed)", {
  skip_if_offline()
  # Marta-Extracted FMC data 
  if (.Platform$OS.type == "unix") {
    skipnum <- 13
  } else {skipnum <- 5}
  df <- read.table("./private/data/remote_sensed/Birdsite_GPP_FMC_pointdrills_GPP.csv",
                   skip = skipnum, 
                   blank.lines.skip = TRUE, 
                   sep = ",",
                   header = TRUE)
  colnames(df)[1:3] <- c("year", "month", "day")
  colnames(df) <- sub("\\.", "", colnames(df))
  longlats <- df[1:2, 5:ncol(df)]
  row.names(longlats) <- df[1:2, 4]
  gppvals <- df[4:nrow(df),-4]
  
  # Kass-Extracted GPP Data
  sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
  points <- sws_sites_2_spdf(sws_sites)[1:10, ]
  tseries <- extract_ts_wald(points,
                             "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc",
                             varname = "GPP",
                             nl = 10) 
  
  expect_equivalent(t(tseries)[, "ARCH1"], gppvals[1:10, "ARCH1"])
  expect_equivalent(t(tseries)[, "ARCH2"], gppvals[1:10, "ARCH2"])
  expect_equivalent(t(tseries)[, "ARCH3"], gppvals[1:10, "ARCH3"])
})

test_that("extract_ts_wald() matches FMC values extracted by Marta (dimensions not transposed)", {
  skip_if_offline()
  # Marta-Extracted FMC data 
  if (.Platform$OS.type == "unix") {skipnum <- 13} else {skipnum <- 5}
  df <- read.table("./private/data/remote_sensed/Birdsite_GPP_FMC_pointdrills_FMC.csv",
                   skip = skipnum, 
                   blank.lines.skip = TRUE, 
                   sep = ",",
                   header = TRUE)
  colnames(df)[1:3] <- c("year", "month", "day")
  colnames(df) <- sub("\\.", "", colnames(df))
  longlats <- df[1:2, 5:ncol(df)]
  row.names(longlats) <- df[1:2, 4]
  fmcvals <- df[4:nrow(df),-4]
  
  # Kass-Extracted FMC Data
  sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
  points <- sws_sites_2_spdf(sws_sites)[1:2, ]
  tseries <- extract_ts_wald(points,
                             "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/FMC/c6/mosaics/fmc_c6_2001.nc",
                             varname = "fmc_mean",
                             nl = 2) 
  tseriesdates <- as.POSIXlt(as.numeric(gsub("X", "", colnames(tseries))), origin = lubridate::origin)
  colnames(tseries) <- as.character(tseriesdates)
  
  expect_equivalent(t(tseries)[, "ARCH1"], fmcvals[1:2, "ARCH1"])
  expect_equivalent(t(tseries)[, "ARCH2"], fmcvals[1:2, "ARCH2"])
  #t(tseries)
  #fmcvals[1:2, row.names(tseries)]
})


