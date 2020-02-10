# Importing Preprocessing Information
scale_matrix <- readRDS("./private/coefficients/4_1_script_scale_matrix.rds")



####################################
# Loading Predictor Data for Region
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)

# gpp_mean
gpp_timemean <- raster("./private/data/remote_sensed/gpp_mean.grd")

# m1bresid
m1b_resid <- brick("./private/data/derived/m1b_resid_Sept6th.grd")
gpp_times <- as_date(names(m1b_resid), tz = "Australia/Canberra", format = "X%Y.%m.%d")

# woody cover #NOT SMOOTHED YET#
woodycover <- brick("./private/data/remote_sensed/woodycover_all_lowres.grd")
woodycover_years <- year(as_date(names(woodycover), tz = "Australia/Canberra", format = "X%Y"))


##########################
# prepare predictor values (scaling, time points and spatial grid)
## Turn each preprocessing step into a function that can be applied to lists of pixel values
prep_gpp_timemean <- function(x){
  z <- scale(x,
      center = scale_matrix["center", "gpp_mean"],
      scale = scale_matrix["scale", "gpp_mean"])
  return(z)
}
prep_log_plus_one_woody_cover <- function(x){
  z <- scale(log(x + 1),
      center = scale_matrix["center", "log_plus_one_woody_cover"],
      scale = scale_matrix["scale", "log_plus_one_woody_cover"])
  return(z)
}
prep_date <- function(x){
  z <- scale(x,
      center = scale_matrix["center", "date"],
      scale = scale_matrix["scale", "date"])
  return(as.vector(z))
}
prep_m1b_resid <- function(x){
  z <- scale(x,
      center = scale_matrix["center", "m1b_resid"],
      scale = scale_matrix["scale", "m1b_resid"])
  return(z)
}

## Apply the above functions
t.gpp_timemean <- calc(gpp_timemean, fun = prep_gpp_timemean)
t.woodycover <- calc(woodycover, fun = prep_log_plus_one_woody_cover)
names(t.woodycover) <- names(woodycover)
t.m1b_resid <- calc(m1b_resid, fun = prep_m1b_resid)
names(t.m1b_resid) <- names(m1b_resid)
# dates prepped later


# spatial:
#   go to coarse resolution: **this should be replicated into the preprocessing for the modelling data
t.woodycover <- projectRaster(t.woodycover, t.gpp_timemean, method = "bilinear")
names(t.woodycover) <- names(woodycover)

# time: Sept6 works for all years except 2001 
# GPP dates restrict to Woody Cover data assuming woody cover constant within a year
gpp_wc_times <- gpp_times[year(gpp_times) %in% woodycover_years]

out_dates <- gpp_wc_times

### Now create/cut data based on the above dates
t.woodycover <- t.woodycover[[paste0("X", year(out_dates)), ]]
names(t.woodycover) <- out_dates

# cut down m1b_resid to have only out_dates
t.m1b_resid <- t.m1b_resid[[ names(t.m1b_resid)[gpp_times %in% out_dates] ]]


# a simple brick of dates given model scaling
dates <- t.m1b_resid
names(dates) <- names(t.m1b_resid)
values(dates) <- rep(prep_date(as.numeric(out_dates)), each = nrow(dates) * ncol(dates))
names(dates) <- out_dates

# interaction term
gppwc_inter <- gpp_timemean * t.woodycover
names(gppwc_inter) <- names(t.woodycover)

# predictors combined nicely and exported:
predictors_cannon_form = list(intercept = 1,
                  gpp_mean = t.gpp_timemean,
                  m1b_resid = t.m1b_resid,
                  log_plus_one_woody_cover = t.woodycover,
                  "gpp_mean:log_plus_one_woody_cover" = gppwc_inter,
                  year = dates)
mapply(function(x, y) writeRaster(x, filename = paste0("./tmpdata/4_1_predictors_cannon_form_", y, ".grd"), overwrite = TRUE),
       predictors_cannon_form[2:6],
       make.names(names(predictors_cannon_form)[2:6]),
       SIMPLIFY = FALSE)

