context("Remote Sensing Data Extraction")
library(testthat); library(maptools); library(raster); library(ncdf4)

test_that("extract_ts_wald() matches FMC values extracted by Marta (dimensions not transposed)", {
  # Marta-Extracted GPP data 
  df <- read.table("./data/Birdsite_GPP_FMC_pointdrills_FMC.csv",
                             skip = 13, 
                             blank.lines.skip = TRUE, 
                             sep = ",",
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
  
  t(tseries)
  fmcvals[1:2, c("year", "month", "day", row.names(tseries))]
  })


test_that("Extraction works for maps with different extents", {
  filelocation <- "http://dapds00.nci.org.au/thredds/dodsC/ub8/au/FMC/c6/mosaics/fmc_c6_2001.nc"
  http://dapds00.nci.org.au/thredds/dodsC/ub8/au/OzWALD/8day/GPP/OzWALD.GPP.2000.nc
})



test_that("Annual precipitation at a fixed location extracted properly", {
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
