# maps of raw data on species observations at the farm level
library(readxl)
library(ggplot2)
library(viridis)
library(sf)
library(ggrepel) # for adding text labels at nice locations


# extract raw data
sites_raw <- as.data.frame(
  read_excel(
    "./private/data/raw/LongTermStudies_SiteTableData_22-03-2019.xlsx",
    sheet = "SWS"
))
sites_simple <- sites_raw[, c(
  "SiteCode", "GrowthType",
  "latitude", "longitude", "elevation",
  "Rmnnt/PltngsSize (ha)", "NativeVegArea500mRadius"
)]
for(i in 3:7){sites_simple[, i] <- as.numeric(sites_simple[, i])}

traits <- as.data.frame(
  read_excel(
    "./private/data/raw/Ikin_SWS_Bird_Traits_updatedApril2017.xlsx",
    sheet = "Ikin_SWS_Bird_Traits"
))
nectar_feeders <- traits$species[which(traits$diet == "Nectar")]
# traits$species[which(traits$diet == "Varied")]
# traits[54, ] # D/usky Woodswallow listed as an insectivore

focal_species <- c("Black-chinned Honeyeater", "Dusky Woodswallow", "Little Lorikeet")
birds_all <- readRDS("./private/data/clean/sws_bird_richness.rds")
sites_obs <- readRDS("./private/data/clean/sws_sites.rds")



# calculate number of observations of each species per farm
focal_list <- split(
  birds_all[, focal_species],
  substr(sites_obs$SiteCode, 1, 4)
)
focal_df <- as.data.frame(do.call(rbind,
  lapply(focal_list, function(a){
    apply(a, 2, function(b){length(which(b > 0))})
  })
))
focal_df$farm <- names(focal_list)

# add richness
focal_list <- split(
  apply(
    birds_all[, colnames(birds_all) %in% nectar_feeders],
    1,
    function(b){length(which(b > 0))}
  ),
  substr(sites_obs$SiteCode, 1, 4)
)
focal_df$mean_richness <- unlist(lapply(focal_list, mean))
focal_df$max_richness <- unlist(lapply(focal_list, max))



# get coordinates of each fram
coord_list <- split(
  sites_simple[, c("latitude", "longitude")],
  substr(sites_simple$SiteCode, 1, 4)
)
coord_df <- as.data.frame(do.call(rbind,
  lapply(coord_list, function(a){apply(a, 2, function(b){mean(b, na.rm = TRUE)})})
))
coord_df$farm <- names(coord_list)
coord_df <- coord_df[!is.na(coord_df$latitude), ]
coord_df <- st_as_sf(coord_df, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")


# merge data and convert to factors for presentation
data_nectarivores <- merge(
  coord_df, focal_df, by = "farm", all.x = TRUE, all.y = FALSE
)
data_nectarivores$bch <- cut(
  data_nectarivores[, "Black-chinned Honeyeater", drop = TRUE],
  c(-1, 0.5, 2.5, 10.5, 50),
  labels = c("0", "1-2", "3-10", "11+")
)
data_nectarivores$dw <- cut(
  data_nectarivores[, "Dusky Woodswallow", drop = TRUE],
  c(-1, 0.5, 2.5, 10.5, 50),
  labels = c("0", "1-2", "3-10", "11+")
)
data_nectarivores$ll <- cut(
  data_nectarivores[, "Little Lorikeet", drop = TRUE],
  c(-1, 0.5, 2.5, 10.5, 50),
  labels = c("0", "1-2", "3-10", "11+")
)

# prep contextual vector data:
majorfeatures <- readRDS("./private/data/GA_principalroads_majorrivers_railsways.rds")  %>% 
  st_transform(st_crs(data_nectarivores)) %>%
  st_crop(data_nectarivores)
builtupareas <- readRDS("./private/data/GA_builtupareas.rds")  %>% 
  st_transform(st_crs(data_nectarivores)) %>%
  st_crop(data_nectarivores)
# in the below CANNOT use an aesethetic colour mapping as ggplot2 only allows one colour mapping and that is needed for the mean species richness in the following layers
contextlyrs <- list(
  geom_sf(data = majorfeatures %>% dplyr::filter(FEATTYPE %in% c("Major Road")),
          inherit.aes = FALSE, col = "grey"), 
  # geom_sf(data = majorfeatures %>% dplyr::filter(FEATTYPE %in% c("Major Watercourse")),
  #         inherit.aes = FALSE, lty = "dotted", col = "grey"),
  geom_sf(data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
          inherit.aes = FALSE, fill = "grey", col = "grey", lwd = 1),
  geom_text_repel(aes(x = cen.X, y = cen.Y, label = NAME),
                  data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
                  inherit.aes = FALSE,
                  nudge_y = 0.1,
                  col = "grey",
                  size = 3)
)


# plot
ggplot(data_nectarivores,
  aes( col = mean_richness, size = mean_richness)
) +
  contextlyrs +
  geom_sf(show.legend = 'point') +
  scale_color_viridis(direction = -1) +
  scale_size(range = c(1, 6)) +
  theme_bw() +
  # coord_fixed() +  # removed by KH: clashses with geom_sf() - creates error
  guides(
    colour = guide_legend("Mean\nSpecies\nRichness"),
    size = guide_legend("Mean\nSpecies\nRichness")
  ) +
  ggtitle("Nectarivores in SWS farms") +
  xlab("Longitude") +
  ylab("Latitude")
ggsave("./private/plots/necatarivore_species_richness.pdf")


ggplot(data_nectarivores) +
  geom_point(
    aes(x = longitude, y = latitude, fill = bch),
    color = "grey30",
    shape = 21,
    size = 4
  ) +
  scale_fill_manual(
    name = "Number of\nObservations",
    values = c("white", viridis(3, begin = 0.35, end = 1, direction = -1))
  ) +
  theme_bw() +
  coord_fixed() +
  ggtitle("Black-chinned Honeyeaters in SWS farms") +
  xlab("Longitude") +
  ylab("Latitude")
ggsave("./private/plots/necatarivores_BCH.pdf")


ggplot(data_nectarivores) +
  geom_point(
    aes(x = longitude, y = latitude, fill = dw),
    color = "grey30",
    shape = 21,
    size = 4
  ) +
  scale_fill_manual(
    name = "Number of\nObservations",
    values = c("white", viridis(3, begin = 0.35, end = 1, direction = -1))
  ) +
  theme_bw() +
  coord_fixed() +
  ggtitle("Dusky Woodswallows in SWS farms") +
  xlab("Longitude") +
  ylab("Latitude")
ggsave("./private/plots/necatarivores_DW.pdf")


ggplot(data_nectarivores) +
  geom_point(
    aes(x = longitude, y = latitude, fill = ll),
    color = "grey30",
    shape = 21,
    size = 4
  ) +
  scale_fill_manual(
    name = "Number of\nObservations",
    values = c("white", viridis(3, begin = 0.35, end = 1, direction = -1))
  ) +
  theme_bw() +
  coord_fixed() +
  ggtitle("Little Lorikeets in SWS farms") +
  xlab("Longitude") +
  ylab("Latitude")
ggsave("./private/plots/necatarivores_LL.pdf")