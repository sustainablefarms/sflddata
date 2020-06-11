## Preparation
invisible(lapply(c("sf", "sp", "raster", "maptools", "rgdal", "ncdf4", "lubridate",
                   "tsibble", "tidyr", "dplyr", "fable"), #fable for the box_cox transformation
       library, character.only = TRUE))
out <- lapply(paste0("./R/", list.files("./R/")), source)

m1b <- readRDS(file = "./private/models/m1b.rds")

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
roi <- extent(points)

# build brick of pg data
pg_brick <- brick_pg(points, 2000:2018) #there are different extents for 2000, 2001 - 2016, and 2017 - 2018. Differences are half a pixel.
pg_brick[[1]] <- resample(pg_brick[[1]], pg_brick[[2]])
pg_brick[[3]] <- resample(pg_brick[[3]], pg_brick[[2]])
pg_brick <- brick(pg_brick)
gpp_brick <- brick_gpp(points, 2000:2018)

writeRaster(pg_brick, filename = "pg_brick_tmp", overwrite = TRUE)
writeRaster(gpp_brick, filename = "gpp_brick_tmp", overwrite = TRUE)

### Cumulative rainfalls
pg_cum_brick <- calc(pg_brick, cumsum); names(pg_cum_brick) <- names(pg_brick)
pg_0to5m <- pg_cum_brick[[(1 + 5 * 31) : nlayers(pg_cum_brick)]] -  pg_cum_brick[[((1 + 5 * 31) : nlayers(pg_cum_brick) ) - 5*31]]
names(pg_0to5m) <- names(pg_cum_brick[[(1 + 5 * 31) : nlayers(pg_cum_brick)]])
pg_1to5m <- pg_0to5m[[-nlayers(pg_0to5m)]]
names(pg_1to5m) <- names(pg_0to5m[[-1]])
pg_24d <- pg_cum_brick[[(1 + 24) : nlayers(pg_cum_brick)]] -  pg_cum_brick[[((1 + 24) : nlayers(pg_cum_brick) ) - 24]]
names(pg_24d) <- names(pg_cum_brick[[(1 + 24) : nlayers(pg_cum_brick)]])

## Seasonal rainfall and gpp
ydaymeds <- function(rasbrick){
  dates <- as_date(names(rasbrick), format = "X%Y.%m.%d", tz = "")
  ydays <- factor(yday(dates))
  ydayidx <- as.numeric(factor(ydays))
  medians <- stackApply(rasbrick, ydayidx, fun = median, na.rm = TRUE)
  outputidx <- as.numeric(gsub("index_", "", names(medians)))
  names(medians) <- paste0("X", levels(ydays)[outputidx])
  orderbyday <- order(as.integer(gsub("X", "", names(medians))))
  medians <- medians[[orderbyday]]
  return(medians)
}
pg_1to5m.ydaymed <- ydaymeds(pg_1to5m)
gpp.ydaymed <- ydaymeds(gpp_brick)
writeRaster(pg_1to5m.ydaymed, filename = "pg_1to5m.ydaymed_tmp.grd", overwrite = TRUE)
writeRaster(gpp.ydaymed, filename = "gpp.ydaymed_tmp.grd", overwrite = TRUE)

## Compute predictions for m1b
m1b_predfordate <- function(date, pg_1to5m, pg_1to5m.ydaymed, pg_24d, gpp.ydaymed){
  stopifnot(length(date) == 1)
  datelayername <- strftime(date, format = "X%Y.%m.%d")
  ydaylayername <- paste0("X", yday(date))
  linpred <- m1b$coefficients[["(Intercept)"]] +
          m1b$coefficients[["pg_1to5m"]] * pg_1to5m[[datelayername]] +
          m1b$coefficients[["I(1/pg_1to5m.ydaymed)"]] * 1 / pg_1to5m.ydaymed[[ydaylayername]] +
          m1b$coefficients[["pg_24d"]] * pg_24d[[datelayername]] +
          m1b$coefficients[["pg_1to5m:I(1/pg_1to5m.ydaymed)"]] * pg_1to5m[[datelayername]] * 1 / pg_1to5m.ydaymed[[ydaylayername]] +
          m1b$coefficients[["pg_1to5m:pg_24d"]] * pg_1to5m[[datelayername]] *  pg_24d[[datelayername]] +
          m1b$coefficients[["I(1/pg_1to5m.ydaymed):pg_24d"]] * (1 / pg_1to5m.ydaymed[[ydaylayername]]) * pg_24d[[datelayername]] +
          m1b$coefficients[["pg_1to5m:I(1/pg_1to5m.ydaymed):pg_24d"]] * pg_1to5m[[datelayername]] * (1 / pg_1to5m.ydaymed[[ydaylayername]]) * pg_24d[[datelayername]]
  
  
  pred.gpp.ratio <- calc(linpred , function(x) fabletools::inv_box_cox(x, lambda = 0.1414141))
  gpp.pred <- gpp.ydaymed[[ydaylayername]] * resample(pred.gpp.ratio, gpp.ydaymed[[ydaylayername]])
  return(gpp.pred)
}

dates <- as_date(names(gpp_brick), format = "X%Y.%m.%d", tz = "")
dates <- dates[dates %in% as_date(names(pg_1to5m), format = "X%Y.%m.%d", tz = "")]
library(parallel)
cl <- makeCluster(2)
clusterExport(cl = cl,
              varlist = c("m1b_predfordate", "yday", "m1b",
                          "pg_1to5m", "pg_1to5m.ydaymed", "pg_24d", "gpp.ydaymed",
                          "resample", "calc")
                          )
gpp.preds <- parLapply(cl = cl, X = dates,
                    fun = function(x) m1b_predfordate(x , pg_1to5m, pg_1to5m.ydaymed, pg_24d, gpp.ydaymed))
gpp.preds <- brick(gpp.preds)
names(gpp.preds) <- dates

## Write Predictions for GPP: all and Sept 6th
writeRaster(gpp.preds, filename = "./private/data/derived/m1b_pred_all.grd", overwrite = TRUE)
writeRaster(gpp.preds[[grep("X.....09.06", names(gpp.preds))]],
            filename = "./private/data/derived/m1b_pred_Sept6th.grd", overwrite = TRUE)



gpp.resids <- gpp_brick[[strftime(dates, format = "X%Y.%m.%d")]] - resample(gpp.preds, gpp_brick)
names(gpp.resids) <- strftime(dates, format = "X%Y.%m.%d")
writeRaster(gpp.resids,
            filename = "./private/data/derived/m1b_resid_all.grd", overwrite = TRUE)
# gpp.pred1 <- m1b_predfordate(as_date(names(gpp_brick), format = "X%Y.%m.%d", tz = "")[[40]],
#                 pg_1to5m, pg_1to5m.ydaymed, pg_24d, gpp.ydaymed)

## Write Residuals for Sept6th
writeRaster(gpp.resids[[grep("X.....09.06", names(gpp.resids))]],
            filename = "./private/data/derived/m1b_resid_Sept6th.grd", overwrite = TRUE)

## Write 90% quantile of residuals for each pixel
resids.qauntiles <- calc(gpp.resids, function(x) stats::quantile(x, na.rm = TRUE, probs = c(0.1, 0.25, 0.5, 0.75, 0.9)))
names(resids.qauntiles) <- c("q0.1", "q0.25", "q0.5", "q0.75", "q0.9")
writeRaster(resids.qauntiles,
            filename = "./private/data/derived/m1b_resid_quantiles.grd", overwrite = TRUE)

resids.mean <- calc(gpp.resids, function(x) base::mean(x, na.rm = TRUE))
writeRaster(resids.mean,
            filename = "./private/data/derived/m1b_resid_mean.grd", overwrite = TRUE)

## Convert above bricks into dataframes for the farm sites
m1b_pred <- t(raster::extract(gpp.preds, points)) %>%
            add_colnames_times_tseries(points$SiteCode)

m1b_resid <- t(raster::extract(gpp.resids, points)) %>%
            add_colnames_times_tseries(points$SiteCode)

## Save Predictions and Residual
saveRDS(m1b_pred, "./private/data/derived/m1b_pred.rds")
saveRDS(m1b_resid, "./private/data/derived/m1b_resid.rds")

# m1b_pred %>%
#   pivot_longer(-times, values_to = "gpp.pred", names_to = "site") %>%
#   inner_join(pggpp, by = c("times", "site")) %>%
#   filter(site == "ARCH1") %>%
#   mutate(yday = yday(times)) %>%
#   filter(yday %in% seq(1, 366, by = 8)) %>%
#   ggplot() +
#   geom_line(aes(x = times, y = gpp.pred)) +
#   geom_point(aes(x = times, y = gpp), col = "blue")

