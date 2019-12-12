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
woodycover <- brick_woodycover(spobj, 2000:2018)
## compute average of buffer for every pixel
wf <- focalWeight(woodycover, 500, type = "circle") 
woodycover_smooth <- focal_bylayer(woodycover, wf, fun = sum)
names(woodycover_smooth) <- names(woodycover)
woodycover_years <- year(as_date(names(woodycover), tz = "Australia/Canberra", format = "X%Y"))


##########################
# prepare predictor values (scaling, time points and spatial grid)
gpp_timemean <- calc(gpp_timemean, fun = prep_gpp_timemean)
gpp_diff <- calc(gpp_diff, fun = prep_gpp_diff)
names(gpp_diff) <- names(gpp)
fmc_mean_diff <- calc(fmc_mean_diff, fun = prep_fmc_diff)
names(fmc_mean_diff) <- names(fmc_mean)
woodycover_smooth <- calc(woodycover_smooth, fun = prep_log_plus_one_woody_cover)
names(woodycover_smooth) <- names(woodycover)
# dates prepped later


# spatial:
#   go to coarse resolution: **this should be replicated into the preprocessing for the modelling data
woodycover_smooth_lr <- projectRaster(woodycover_smooth, fmc_mean, method = "bilinear")
names(woodycover_smooth_lr) <- names(woodycover_smooth)

# time: work out dates available, cut down all data to those dates
#  FMC and GPP are produced at the same time points,
# then restrict to Woody Cover data assuming woody cover constant within a year
fmc_gpp_times <- gpp_times[gpp_times %in% fmc_times]
fmc_gpp_wc_times <- fmc_gpp_times[year(fmc_gpp_times) %in% woodycover_years]

# cut down dates to bimonthly for computational reasons
approx_desired_dates <- seq(fmc_gpp_wc_times[[1]],
                            to = fmc_gpp_wc_times[[length(fmc_gpp_wc_times)]],
                            by = "2 month")
out_dates <- fmc_gpp_wc_times[vapply(approx_desired_dates, function(x) which.min(abs(fmc_gpp_wc_times - x)), FUN.VALUE = 3)]

### Now create/cut data based on the above dates
# a stack of woody cover layers repeated for each time point
woodycover_smooth_extra_dates_stack <- woodycover_smooth_lr[[paste0("X", year(out_dates)), ]]
names(woodycover_smooth_extra_dates_stack) <- out_dates

# cut down fmc_diff and gpp_diff to have only the times of interest:
gpp_diff <- gpp_diff[[ names(gpp_diff)[gpp_times %in% out_dates] ]]
fmc_mean_diff <- fmc_mean_diff[[ names(fmc_mean_diff)[fmc_times %in% out_dates] ]]

# a simple brick of dates given model scaling
dates <- gpp_diff
names(dates) <- names(gpp_diff)
values(dates) <- rep(prep_date(as.numeric(out_dates)), each = nrow(dates) * ncol(dates))
names(dates) <- out_dates

# interaction term
gppwc_inter <- gpp_timemean * woodycover_smooth_extra_dates_stack

# predictors combined nicely and exported:
predictors_cannon_form = list(intercept = 1,
                  gpp_mean = gpp_timemean,
                  gpp_diff = gpp_diff,
                  fmc_diff = fmc_mean_diff,
                  woody_cover = woodycover_smooth_extra_dates_stack,
                  "gpp_mean:woody_cover" = gppwc_inter,
                  year = dates)
mapply(function(x, y) writeRaster(x, filename = paste0("./tmpdata/predictors_cannon_form_", y, ".nc")),
       predictors_cannon_form[2:6],
       make.names(names(predictors_cannon_form)[2:6]),
       SIMPLIFY = FALSE)


#################### applying model prediction #############
boral_coefficients_matrix <- readRDS("./private/coefficients/boral_coefficients_matrix.rds")

# weight predictors by coefficients and then sum
wght_sum_pnorm <- function(rowname, coefficient_mat, predictors_cannon_form){
  coefficients <- coefficient_mat[rowname, ]
  wght_pred <- mapply("*", coefficients, predictors_cannon_form, SIMPLIFY = FALSE)
  linearpred <- Reduce("+", wght_pred)
  prediction <- calc(linearpred, pnorm)
  names(prediction) <- out_dates
  writeRaster(prediction,
              filename = paste0("./tmpdata/pred_", make.names(rowname), ".nc"),
              varname = make.names(rowname),
              longname = rowname,
              zname = "time",
              overwrite = TRUE)
  return(prediction)
}

pred <- lapply(row.names(boral_coefficients_matrix), 
              wght_sum_pnorm,
              coefficient_mat = boral_coefficients_matrix,
              predictors_cannon_form = predictors_cannon_form)
names(pred) <- row.names(boral_coefficients_matrix)

# load predictions if needed:
tmpdatadir <- "/home/kassel/tmpdata/"
out_dates <- readRDS(paste0(tmpdatadir, "out_dates.rds"))
boral_coefficients_matrix <- readRDS("./private/coefficients/boral_coefficients_matrix.rds")
pred <- lapply(row.names(boral_coefficients_matrix),
                function(x) {
                  b <- brick(paste0(tmpdatadir, "pred_", make.names(x), ".nc"))
                  b@z <- as.list(out_dates)
                  names(b) <- out_dates
                  return(b)
                })
names(pred) <- row.names(boral_coefficients_matrix)

# Render predictions as gifs
library(ggplot2)
library(rasterVis)
library(viridis)
gplot(subset(pred[[1]], 1:2)) +
  geom_tile(aes(fill = value)) +
  facet_grid(~ variable) + 
  scale_fill_viridis() +
  coord_fixed()
levelplot(subset(pred[[1]], 1:2))


library(animation)
create_pred_gif <- function(x, name) {
  saveGIF({
    for (i in 1:nlayers(x)) plot(raster(x, i), zlim = c(0, 1), main = paste(name, names(x)[i]))
    },
    movie.name = paste0(make.names(name), ".gif"))
}
#create_pred_gif(pred[[1]], "Australian Raven")

lapply(names(pred), function(x) create_pred_gif(pred[[x]], x))




##################################################
## Compare Predictions to Predictions from Training Data ##
##################################################
# analysis of SWS angry bird dataset
library(boral)
source("./functions/order_boral.R")
source("./functions/return_current_time.R") # necessary function for file out


# import data
bird_richness <- readRDS("./private/data/clean/sws_bird_richness.rds") # contains all bird spp
birds <- readRDS("./private/data/clean/sws_birds.rds") # contains only common bird spp
sites_rs <- readRDS("./private/data/clean/sws_sites_rs.rds")
sites_rs$log_plus_one_woody_cover <- log(sites_rs$woody_cover + 1)
traits <- readRDS("./private/data/clean/sws_traits.rds")

# organise bird data
# make binary
for(i in 1:ncol(birds)){
  birds[which(birds[, i] > 0), i] <- 1
}
for(i in 1:ncol(bird_richness)){
  bird_richness[which(bird_richness[, i] > 0), i] <- 1
}

# nm_col <- which(colnames(birds) == "Noisy Miner")
# birds <- birds[, -nm_col]


# make predictor matrix
predictors <- data.frame(
  # type = sites$GrowthType,
  # noisy_miners = birds[, nm_col],
  gpp_mean = scale(sites_rs$gpp_mean),
  gpp_diff = scale(sites_rs$gpp_diff),
  fmc_mean = scale(sites_rs$fmc_mean),
  fmc_diff = scale(sites_rs$fmc_diff),
  woody_cover = scale(sites_rs$log_plus_one_woody_cover),
  year = scale(as.numeric(sites_rs$date))
)

na_check <- which(apply(
  cbind(predictors, birds),
  1,
  function(a){all(!is.na(a))}
))
# length(na_check)

predictors <- predictors[na_check, ]
X <- model.matrix(
  ~ gpp_mean * woody_cover + fmc_diff + gpp_diff + year,
  data = predictors)[, -1]

species <- row.names(boral_coefficients_matrix)[[1]]
pman <- pnorm(boral_coefficients_matrix[species, "intercept"] + 
    X[, colnames(boral_coefficients_matrix)[-1]] %*%
     boral_coefficients_matrix[species, -1] )
pman <- cbind(data.frame(prediction = pman), SiteCode = sites_rs[na_check, "SiteCode"], date = sites_rs[na_check, "date"])
pman_site = pman[pman[, "SiteCode"] == "WILS1", ]

sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
points <- sws_sites_2_spdf(sws_sites)
tmpdatadir <- "/home/kassel/tmpdata/"
prasbrick <- brick(paste0(tmpdatadir, "pred_", make.names(species), ".nc"))
pred_dates <- readRDS(paste0(tmpdatadir, "out_dates.rds"))
names(prasbrick) <- pred_dates
pras_site <- extract(prasbrick, points["WILS1",])
pras_site <- data.frame(raspred = t(pras_site), date = pred_dates)
pras_site[vapply(pman_site$date, function(x) which.min(abs(pred_dates - x)), FUN.VALUE = 3), ]
# values seem to be in the correct ball park for ARCH1, LIND1 and WILS1 but there isn't much variation between dates and the raster predictions aren't getting to it.
