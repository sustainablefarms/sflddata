# script for plots for meeting 19 December 2019
library(ggplot2)
library(rasterVis)
library(viridis)
library(ggrepel)
library(sf)
source("./functions/sites_2_sp_points.R")

swspoints <- sws_sites_2_sf(readRDS("./private/data/clean/sws_sites.rds"))
majorfeatures <- readRDS("./private/data/GA_principalroads_majorrivers_railsways.rds")  %>% 
  st_transform(st_crs(swspoints)) %>%
  st_crop(swspoints)
builtupareas <- readRDS("./private/data/GA_builtupareas.rds")  %>% 
  st_transform(st_crs(swspoints)) %>%
  st_crop(swspoints)

majorfeatures %>%
  dplyr::filter(FEATTYPE != "Watercourse Area") %>%
  ggplot() +
  geom_sf(aes(col = FEATTYPE), stat = "sf") +
  #scale_colour_viridis(discrete = TRUE) +
  geom_sf(data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
          inherit.aes = FALSE, stat = "sf", fill = "black", col = "black", lwd = 1) +
  coord_sf() +
  geom_text_repel(aes(x = cen.X, y = cen.Y, label = NAME),
                  data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
                  inherit.aes = FALSE, 
                  nudge_y = 0.1,
                  col = "black",
                  size = 3) +
  xlab("Longitude") + ylab("Latitude")
ggsave("./private/plots/sws_region_context_noswspoints.pdf")


###########################################################

majorfeatures %>%
  dplyr::filter(FEATTYPE != "Watercourse Area") %>%
  ggplot() +
  geom_sf(aes(col = FEATTYPE), stat = "sf") +
  geom_sf(data = swspoints, inherit.aes = FALSE, stat = "sf", col = "red") + 
  geom_sf(data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
          inherit.aes = FALSE, stat = "sf", fill = "black", col = "black", lwd = 1) +
  coord_sf() +
  geom_text_repel(aes(x = cen.X, y = cen.Y, label = NAME),
                  data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
                  inherit.aes = FALSE, 
                  nudge_y = 0.1,
                  col = "black",
                  size = 3) +
  xlab("Longitude") + ylab("Latitude")
ggsave("./private/plots/sws_region_context_swspoints.pdf")
