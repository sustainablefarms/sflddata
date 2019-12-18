# Script to generate nice plots of regional predictions
invisible(lapply(c("raster", "rasterVis", "maptools", "rgdal", "ncdf4", "lubridate", "sf", "dplyr"),
                 library, character.only = TRUE))
out <- lapply(paste0("./functions/", list.files("./functions/")), source)


# load predictions from temporary storage (too big to save):
tmpdatadir <- "C:/UserData/hingeek/tmpdata/"
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

# load sws sites:
sws_sites <- sws_sites_2_sf(readRDS("./private/data/clean/sws_sites.rds"))

# line data
majorfeatures <- readRDS("./private/data/GA_principalroads_majorrivers_railsways.rds")  %>% 
  st_transform(st_crs(pred[[1]])) %>%
  st_crop(pred[[1]]) %>%
  mutate(FEATTYPE = recode(FEATTYPE, "Watercourse Area" = "Major Watercourse"))


################
# Plot predictions as animations
library(ggplot2)
library(rasterVis)
library(viridis)
library(gganimate)
# function creating plotting object
animate1 <- function(x, speciesname, layers = 1:nlayers(x)){
  plt <- gplot(subset(x, layers)) +
    geom_tile(aes(fill = value)) +
    scale_fill_viridis() +
    geom_sf(aes(col = FEATTYPE), data = majorfeatures, inherit.aes = FALSE, stat = "sf") +
    geom_sf(data = sws_sites, inherit.aes = FALSE, stat = "sf", col = "red") + 
    coord_sf() +
    transition_manual(variable) +
    ggtitle(paste("Prediction of", speciesname, "at {current_frame}")) +
    xlab("Longitude") + ylab("Latitude")
  anim_save(paste0(tmpdatadir, "gifs/", make.names(speciesname), ".gif"), animation = plt)
  return(NULL)
}

animate1.l <- mapply(animate1, pred[20:length(pred)], names(pred)[20:length(pred)], SIMPLIFY = FALSE)


plt <- gplot(subset(pred[[1]], 1:2)) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis() +
  geom_sf(aes(col = FEATTYPE), data = majorfeatures, inherit.aes = FALSE, stat = "sf") +
  geom_sf(data = sws_sites, inherit.aes = FALSE, stat = "sf", col = "red") +
  coord_sf() +
  facet_grid(.data$variable) + 
#  transition_time(as_date(variable,  tz = "Australia/Canberra", format = "X%Y.%m.%d")) +
#  ggtitle("Prediction of", paste(names(pred)[[1]], "at {current_frame}")) +
  xlab("Longitude") + ylab("Latitude")

fortify.Raster.rasterVis <- function(x, maxpixels = 50000){
  nl <- nlayers(x)
  x <- sampleRegular(x, maxpixels, asRaster=TRUE)
  coords <- xyFromCell(x, seq_len(ncell(x)))
  ## Extract values 
  dat <- stack(as.data.frame(getValues(x)))
  names(dat) <- c('value', 'variable')
  
  dat <- cbind(coords, dat)
  return(dat)
}
dat <- fortify.Raster.rasterVis(subset(pred[[1]], 1:2)) #warning! samples the data
ndat <- dat %>%
  dplyr::mutate(dates = as_date(as.character(variable), tz = "Australia/Canberra", format = "X%Y.%m.%d"))
ggplot(aes(x = x, y = y), data = ndat) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis() +
  geom_sf(aes(col = FEATTYPE), data = majorfeatures, inherit.aes = FALSE, stat = "sf") +
  geom_sf(data = sws_sites, inherit.aes = FALSE, stat = "sf", col = "red") +
  coord_sf() +
  transition_time(dates) + 
  #  transition_time(as_date(variable,  tz = "Australia/Canberra", format = "X%Y.%m.%d")) +
  #  ggtitle("Prediction of", paste(names(pred)[[1]], "at {current_frame}")) +
  xlab("Longitude") + ylab("Latitude")

sws_site_dates <- readRDS("./private/data/clean/sws_sites.rds") %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude))

ggplot(aes(x = x, y = y), data = ndat) +
  geom_tile(aes(fill = value)) +
  scale_fill_viridis() +
  geom_sf(aes(col = FEATTYPE), data = majorfeatures, inherit.aes = FALSE, stat = "sf") +
#  geom_sf(data = sws_sites, inherit.aes = FALSE, stat = "sf", col = "red") +
  coord_sf() +
  transition_time(dates) +
  geom_point(aes(x = longitude, y = latitude), data = sws_site_dates, inherit.aes = FALSE, col = "red") +
  transition_time(SurveyDate) +
  #  ggtitle("Prediction of", paste(names(pred)[[1]], "at {current_frame}")) +
  xlab("Longitude") + ylab("Latitude")


ggplot(sws_site_dates) +
  geom_point(aes(x = longitude, y = latitude), inherit.aes = FALSE, col = "red") +
  transition_states(SurveyDate) +
  enter_fade() +
  #exit_fade() +
  exit_recolour(fill = "grey") + 
  #ggtitle("Prediction of", paste(names(pred)[[1]], "at {frame_time}")) +
  xlab("Longitude") + ylab("Latitude")
