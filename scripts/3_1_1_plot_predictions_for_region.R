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
  return(plt)
}

animate1.l <- mapply(animate1, pred[20:length(pred)], names(pred)[20:length(pred)], SIMPLIFY = FALSE)

mapply(function(x, y) 
  anim_save(paste0(tmpdatadir, "gifs/", y, ".gif"), animation = x),
            animate1.l, make.names(names(animate1.l)), SIMPLIFY = FALSE)


# plt <- gplot(subset(pred[[1]], 1:10)) +
#   geom_tile(aes(fill = value)) +
#   scale_fill_viridis() +
#   geom_sf(aes(col = FEATTYPE), data = majorfeatures, inherit.aes = FALSE, stat = "sf") +
#   geom_sf(data = sws_sites, inherit.aes = FALSE, stat = "sf", col = "red") + 
#   coord_sf() +
#   transition_manual(variable) +
#   ggtitle("Prediction of", paste(names(pred)[[1]], "at {current_frame}")) +
#   xlab("Longitude") + ylab("Latitude")


