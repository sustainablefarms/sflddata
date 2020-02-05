# script for plotting data for Victoria
library(ggplot2)
library(rasterVis)
library(viridis)
library(ggrepel)
library(sf)

# Example of how to get Australian State data from a geodatabase:
# st_layers("./data_raw/AustralianStates.gdb") # to check layers
# ausstates <- st_read("./data_raw/AustralianStates.gdb", layer = "States")[, 1]
# colnames(ausstates)[1] <- "state" # for prettiness reasons
# ausstates <- rmapshaper::ms_simplify(ausstates) # keeps 5% of points by default
# ausstates <- sf::st_cast(ausstates, "MULTIPOLYGON") # convert from line to polygon
# saveRDS(ausstates, "ausstates.rds")

source("./functions/sites_2_sp_points.R")

vicpoints <- read.csv("./private/data/clean/vic_locations.csv")
# apply(vicpoints[, 5:6], 2, range)
majorfeatures <- readRDS("./private/data/basemaps/GA_principalroads_majorrivers_railsways.rds")  %>%
  # st_transform(st_crs(vicpoints)) %>%
  st_crop(xmin = 143.5, xmax = 150, ymin = -39.3, ymax = -35)
builtupareas <- readRDS("./private/data/basemaps/GA_builtupareas.rds")  %>%
  # st_transform(st_crs(vicpoints)) %>%
  st_crop(xmin = 143.5, xmax = 150, ymin = -39.3, ymax = -35)

# large_areas <- builtupareas %>%
#   dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.95))
large_areas <- builtupareas[builtupareas$NAME %in%
  c("MELBOURNE", "WODONGA", "ALBURY", "GEELONG", "BENDIGO", "TRARALGON"), ]
large_areas$pretty_names <- tools::toTitleCase(tolower(large_areas$NAME))
large_areas$pretty_names <- unlist(lapply(
  strsplit(large_areas$pretty_names, " "),
  function(a){paste(a, collapse = "\n")}
))

ausstates <- readRDS("./private/data/basemaps/ausstates.rds")

region_poly <- data.frame(
  x = range(vicpoints$Long)[c(1, 2, 2, 1)],
  y = range(vicpoints$Lat)[c(1, 1, 2, 2)]
)

aus <- ggplot() +
  geom_sf(data = ausstates, fill = "grey90") +
  geom_polygon(data = region_poly, mapping = aes(x = x, y = y)) +
  theme_bw()


ggplot() +
  geom_sf(data = ausstates, fill = "grey90", color = NA) +
  geom_sf(
    data = majorfeatures[majorfeatures$FEATTYPE == "Major Road", ],
    stat = "sf",
    color = "grey70"
  ) +
  geom_sf(
    data = builtupareas,
    inherit.aes = FALSE,
    stat = "sf",
    fill = "grey70",
    col = "grey70",
    lwd = 1
  ) +
  geom_sf(data = ausstates, fill = "NA", color = "black") +
  geom_text_repel(aes(x = cen.X, y = cen.Y, label = pretty_names),
    data = large_areas,
    inherit.aes = FALSE,
    nudge_y = 0.05,
    col = "black",
    size = 4,
    segment.size = 0
  ) +
  # geom_point(data = vicpoints,
  #   aes(x = Long, y = Lat),
  #   inherit.aes = FALSE,
  #   col = "black", # RColorBrewer::brewer.pal(5, "RdBu")[1], # "grey30",
  #   size = 5,
  #   alpha = 0.5
  # ) +
  geom_polygon(data = region_poly, mapping = aes(x = x, y = y), color = "black", fill = "black") +
  annotate("text", x = 146.3, y = -37.5, label = "STUDY REGION", size = 6, hjust = 0) +
  annotate("text", x = 147.5, y = -35.7, label = "NEW SOUTH WALES", size = 6, color = "white", fontface = "bold") +
  annotate("text", x = 147.5, y = -36.4, label = "VICTORIA", size = 6, color = "white", fontface = "bold") +
  coord_sf(expand = FALSE) +
  xlim(x = c(143.5, 149)) +
  scale_y_continuous(
    limits = c(-39.3, -35.5),
    breaks = seq(-39, -36, 1)
  ) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude")
ggsave("./private/plots/victoria_region_map.pdf")
