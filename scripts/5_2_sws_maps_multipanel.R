# script for plots for meeting 19 December 2019
library(ggplot2)
library(rasterVis)
library(viridis)
library(ggrepel)
library(sf)

source("./R/sites_2_sp_points.R")

swspoints <- sws_sites_2_sf(readRDS("./private/data/clean/sws_sites.rds"))
majorfeatures <- readRDS("./private/data/basemaps/GA_principalroads_majorrivers_railsways.rds")  %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_crop(xmin = 146.5, xmax = 148.5, ymin = -36, ymax = -34.5)

# resample to a smaller resolution
water <- rmapshaper::ms_simplify(
  majorfeatures[majorfeatures$FEATTYPE == "Major Watercourse", ]
)
roads <- rmapshaper::ms_simplify(
  majorfeatures[majorfeatures$FEATTYPE == "Major Road", ]
)

builtupareas <- readRDS("./private/data/basemaps/GA_builtupareas.rds")  %>%
  st_transform("+proj=longlat +datum=WGS84 +no_defs") %>%
  st_crop(xmin = 146.5, xmax = 148.5, ymin = -36, ymax = -34.5)

large_areas <- builtupareas %>%
  dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8))
large_areas$pretty_names <- tools::toTitleCase(tolower(large_areas$NAME))
large_areas$pretty_names <- unlist(lapply(
  strsplit(large_areas$pretty_names, " "),
  function(a){paste(a, collapse = "\n")}
))
large_areas$pretty_names[large_areas$pretty_names == "Harden\n-\nMurrumburrah"] <-
  "Harden-\nMurrumburrah"

detailed_map <- ggplot(majorfeatures) +
  # geom_sf(
  #   data = water,
  #   stat = "sf",
  #   color = RColorBrewer::brewer.pal(3, "RdBu")[3]
  # ) +
  geom_sf(
    data = roads,
    stat = "sf",
    color = "grey70"
  ) +
  geom_sf(
    data = large_areas,
    inherit.aes = FALSE,
    stat = "sf",
    fill = "grey70",
    col = "grey70",
    lwd = 1
  ) +
  geom_sf(data = swspoints,
    inherit.aes = FALSE,
    stat = "sf",
    col = "black", # RColorBrewer::brewer.pal(5, "RdBu")[1], # "grey30",
    size = 4,
    alpha = 0.5
  ) +
  # geom_text_repel(aes(x = cen.X, y = cen.Y, label = pretty_names),
  #   data = large_areas,
  #   inherit.aes = FALSE,
  #   nudge_y = 0.05,
  #   col = "black",
  #   size = 4,
  #   segment.size = 0
  # ) +
  coord_sf(expand = FALSE) +
  xlim(c(146.5, 148.5)) +
  scale_y_continuous(
    limits = c(-36, -34.5),
    breaks = seq(-36, -34.5, 0.5)
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    # panel.background = element_rect(fill = "#d8edc5"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_line(color = "white")
  )
# saveRDS(detailed_map, "./private/data/basemaps/SWS_ggplot.rds")
# detailed_map <- readRDS("./private/data/basemaps/SWS_ggplot.rds")
# ggsave("./private/plots/sws_region_detailed_map.pdf")

# add Australia map next
ausstates <- readRDS("./private/data/basemaps/ausstates.rds")
inset_poly <- data.frame(
  x = c(146.5, 148.5, 148.5, 146.5),
  y = rep(c(-36, -34.5), each = 2)
)
aus <- ggplot() +
  geom_sf(data = ausstates, fill = "white", color = "grey40") +
  geom_polygon(
    data = inset_poly,
    mapping = aes(x = x, y = y),
    fill = "black",
    color = NA
  ) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA),
    plot.background = element_rect(fill = NA)
  )
  # theme_bw()


# use patchwork to merge together
library(patchwork)
layout <- c(
  area(t = 1, l = 2, b = 5, r = 5),
  area(t = 2, l = 1, b = 3, r = 2)
)
detailed_map + aus + plot_layout(design = layout, widths = 1)
ggsave