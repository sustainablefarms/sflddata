# script for plots for meeting 19 December 2019
library(sf)
library(rmapshaper)

library(ggplot2)
library(rasterVis)
library(viridis)
library(ggrepel)
library(patchwork)

library(readxl)

source("./R/sites_2_sp_points.R")

# add site points at the farm level, offset by growth type
# swspoints <- sws_sites_2_sf(readRDS("./private/data/clean/sws_sites.rds"))
swspoints <- as.data.frame(read_excel(
  "./private/data/raw/LongTermStudies_SiteTableData_22-03-2019.xlsx",
  sheet = "SWS"))
swspoints$latitude <- as.numeric(swspoints$latitude)
swspoints$longitude <- as.numeric(swspoints$longitude)

sws_list <- split(swspoints[, c("longitude", "latitude", "GrowthType")], swspoints$FarmUnit)
sws_list <- lapply(seq_along(sws_list), function(a){
  data.frame(
    farm = names(sws_list)[a],
    growth_type = unique(sws_list[[a]]$GrowthType),
    latitude = mean(sws_list[[a]]$latitude, na.rm = TRUE),
    longitude = mean(sws_list[[a]]$longitude, na.rm = TRUE)
  )
})
sws_df <- as.data.frame(do.call(rbind, sws_list))
sws_df <- sws_df[sws_df$growth_type != "NULL", ]

sws_farms <- do.call(rbind,
  lapply(
    split(sws_df[, 3:4], sws_df$farm),
    function(a){a[1, ]}
  ))

spacing_horizontal <- 0.02
spacing_vertical <- 0.015
site_type <- sws_df$growth_type == "planting"
sws_df$longitude[site_type] <- sws_df$longitude[site_type] - spacing_horizontal
sws_df$latitude[site_type] <- sws_df$latitude[site_type] + spacing_vertical
site_type <- sws_df$growth_type == "coppiced regrowth"
sws_df$longitude[site_type] <- sws_df$longitude[site_type] + spacing_horizontal
sws_df$latitude[site_type] <- sws_df$latitude[site_type] + spacing_vertical

site_type <- sws_df$growth_type == "natural regrowth"
sws_df$longitude[site_type] <- sws_df$longitude[site_type] - spacing_horizontal
sws_df$latitude[site_type] <- sws_df$latitude[site_type] - spacing_vertical
site_type <- sws_df$growth_type == "oldgrowth"
sws_df$longitude[site_type] <- sws_df$longitude[site_type] + spacing_horizontal
sws_df$latitude[site_type] <- sws_df$latitude[site_type] - spacing_vertical


# plot features
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
  geom_point(data = sws_df,
    aes(x = longitude, y = latitude, color = growth_type),
    inherit.aes = FALSE,
    # col = "black", # RColorBrewer::brewer.pal(5, "RdBu")[1], # "grey30",
    shape = 15,
    size = 2
    # alpha = 0.5
  ) +
  geom_point(data = sws_farms,
    aes(x = longitude, y = latitude),
    inherit.aes = FALSE,
    col = "black", # RColorBrewer::brewer.pal(5, "RdBu")[1], # "grey30",
    shape = 0,
    size = 5
  ) +
  coord_sf(expand = FALSE) +
  xlim(c(146.5, 148.5)) +
  scale_y_continuous(
    limits = c(-36, -34.5),
    breaks = seq(-36, -34.5, 0.5)
  ) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "", y = "", color = "Vegetation Type") +
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
    fill = NA,
    color = "black",
    size = 2
  ) +
  annotate("text", label = "VIC", x = 146, y = -37, size = 8, color = "grey70") +
  annotate("text", label = "NSW", x = 146, y = -34, size = 8, color = "grey70") +
  annotate("text", label = "ACT", x = 149.5, y = -34.8, size = 6, color = "grey70") +
  annotate("text", label = "STUDY\nREGION", x = 147.5, y = -35.25, size = 4, color = "black") +
  xlab("Longitude") +
  ylab("Latitude") +
  xlim(c(145, 151)) +
  ylim(c(-37.5, -33.5)) +
  theme_bw() +
  theme(
    panel.border = element_rect(color = "grey40", fill = NA),
    plot.background = element_rect(fill = NA)
  )



# use patchwork to merge together
layout <- c(
  area(t = 1, l = 2, b = 5, r = 5),
  area(t = 2, l = 1, b = 3, r = 2)
)
detailed_map + aus + plot_layout(design = layout, widths = 1)
ggsave("./private/plots/SWS_map_by_farm_AUS_inset.pdf")