# First attempt to build a map of the predicted values from Boral modelling

# Importing Preprocessing Information
scale_matrix <- readRDS("./private/coefficients/scale_matrix.rds")

## Turn each preprocessing step into a function that can be applied to lists of pixel values
prep_gpp_timemean <- function(x){
  z <- scale(x,
      center = scale_matrix["centre", "gpp_mean"],
      scale = scale_matrix["scale", "gpp_mean"])
  return(z)
}
prep_gpp_diff <- function(x){
  z <- scale(x,
      center = scale_matrix["centre", "gpp_diff"],
      scale = scale_matrix["scale", "gpp_diff"])
  return(z)
}
prep_fmc_diff <- function(x){
  z <- scale(x,
      center = scale_matrix["centre", "fmc_diff"],
      scale = scale_matrix["scale", "fmc_diff"])
  return(z)
}
prep_log_plus_one_woody_cover <- function(x){
  z <- scale(log(x + 1),
      center = scale_matrix["centre", "log_plus_one_woody_cover"],
      scale = scale_matrix["scale", "log_plus_one_woody_cover"])
  return(z)
}
prep_date <- function(x){
  z <- scale(x,
      center = scale_matrix["centre", "date"],
      scale = scale_matrix["scale", "date"])
  return(as.vector(z))
}


####################################
# Loading Predictor Data for Region
library(raster);library(maptools);library(rgdal);library(ncdf4);library(lubridate)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

# Construct Region Desired
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)

# gpp
gpp <- brick_gpp(points, 2000:2018) #these years must be the same as used in the extracted data due to use of time mean
gpp_timemean <- mean(gpp)
gpp_diff <- gpp - gpp_timemean
names(gpp_diff) <- names(gpp)
gpp_times <- as_date(names(gpp), tz = "Australia/Canberra", format = "X%Y.%m.%d")

# fmc
fmc_mean <- brick_fmc(points, 2001:2019) #these years must be the same as used in the extracted data due to use of time mean
fmc_mean_timemean <- mean(fmc_mean, na.rm = TRUE)
warning("Some locations have very few FMC values available")
fmc_mean_diff <- fmc_mean - fmc_mean_timemean
names(fmc_mean_diff) <- names(fmc_mean)
fmc_times <- as_date(names(fmc_mean), tz = "Australia/Canberra", format = "X%Y.%m.%d")

# gpp and fmc have been interpreted by raster as having ever so slightly different resolutions
# this is due to floating point arithmetic. The following fixes it
extent(gpp) <- extent(fmc_mean)
extent(gpp_timemean) <- extent(fmc_mean)
extent(gpp_diff) <- extent(fmc_mean)


# woody cover
spobj <- buffer(spTransform(points, CRS("+init=epsg:3577")), 1000)
woodycover <- brick_woodycover(spobj, 2002)
## compute average of buffer for every pixel
wf <- focalWeight(woodycover, 500, type = "circle") 
woodycover_smooth <- focal_bylayer(woodycover, wf, fun = sum)

# convert woody cover smooth to the resolution of the other data sets



##########################
# prepare predictor values (scaling, time points and spatial grid)
gpp_timemean <- calc(gpp_timemean, fun = prep_gpp_timemean)
gpp_diff <- calc(gpp_diff, fun = prep_gpp_diff)
names(gpp_diff) <- names(gpp)
fmc_mean_diff <- calc(fmc_mean_diff, fun = prep_fmc_diff)
names(fmc_mean_diff) <- names(fmc_mean)
woodycover_smooth <- calc(woodycover_smooth, fun = prep_log_plus_one_woody_cover)
# dates later


# spatial:
#   go to coarse resolution: **this should be replicated into the preprocessing for the modelling data
woodycover_smooth_lr <- projectRaster(woodycover_smooth, fmc_mean, method = "bilinear")

# time: work out dates available, cut down all data to those dates
#  FMC and GPP are produced at the same time points,
# then restrict to Woody Cover data assuming woody cover constant within a year
fmc_gpp_times <- gpp_times[gpp_times %in% fmc_times]
fmc_gpp_wc_times <- fmc_gpp_times[year(fmc_gpp_times) %in% 
                 year(as_date(names(woodycover_smooth), tz = "Australia/Canberra", format = "X%Y"))]

### Now create/cut data based on the above dates
# a stack of woody cover layers repeated for each time point
woodycover_smooth_extra_dates_stack <- woodycover_smooth_lr[[paste0("X", year(fmc_gpp_wc_times)), ]]

# cut down fmc_diff and gpp_diff to have only the times of interest:
gpp_diff <- gpp_diff[[ names(gpp_diff)[gpp_times %in% fmc_gpp_wc_times] ]]
fmc_mean_diff <- fmc_mean_diff[[ names(fmc_mean_diff)[fmc_times %in% fmc_gpp_wc_times] ]]

# a simple brick of dates given model scaling
dates <- gpp_diff
names(dates) <- names(gpp_diff)
values(dates) <- rep(prep_date(as.numeric(fmc_gpp_wc_times)), each = nrow(dates) * ncol(dates))


# interaction term
gppwc_inter <- gpp_timemean * woodycover_smooth_extra_dates_stack
predictors_cannon_form = list(intercept = 1,
                  gpp_mean = gpp_timemean,
                  gpp_diff = gpp_diff,
                  fmc_diff = fmc_mean_diff,
                  woody_cover = woodycover_smooth_extra_dates_stack,
                  "gpp_mean:woody_cover" = gppwc_inter,
                  year = dates)


#################### applying model prediction
boral_coefficients_matrix <- readRDS("./private/coefficients/boral_coefficients_matrix.rds")

# weight predictors by coefficients and then sum
out <- mapply("*", boral_coefficients_matrix[1, ], predictors_cannon_form, SIMPLIFY = FALSE)
linearpred <- Reduce("+", out)
predictedvalue <- calc(linearpred, pnorm)
names(predictedvalue) <- fmc_gpp_wc_times
plot(subset(predictedvalue, c("X2001.06.02", "X2001.12.19")))
