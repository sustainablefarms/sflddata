# script to process spatial data for display in the farm_biodiversity_app

library(sf)
library(rmapshaper)
library(ggplot2)
library(ggtext)
library(plotly)

# ABS SA2 regions

abs_regions <- read_sf("./data/spatial_raw/ABS_sa2_2016_aust_shape/SA2_2016_AUST.shp")

sa2_regions <- c(
  "Dubbo Region", "Dubbo - West", "Dubbo - East" , "Dubbo - South",
  "Mudgee", "Mudgee Region - West", "Mudgee Region - East",
  "Wellington",
  "Parkes (NSW)", "Parkes Region",
  "Orange Region", "Orange", "Orange - North",
  "Bathurst Region", "Bathurst", "Bathurst - East",
  "Oberon", "Forbes", "Blayney",
  "Cowra Region", "Cowra",
  "Grenfell",
  "Young", "Young Region",
  "Goulburn", "Goulburn Region",
  "Yass", "Yass Region",
  "Temora", "Cootamundra", "Junee", "Gundagai",
  "Narrandera",
  "Wagga Wagga Region", "Wagga Wagga - West", "Wagga Wagga - North",
  "Wagga Wagga - South", "Wagga Wagga - East",
  "Tocumwal - Finley - Jerilderie",
  "Corowa Region", "Corowa", "Rutherglen",
  "Albury Region", "Albury - South", "Albury - North",
  "Lavington", "Albury - East", "Tumbarumba",
  "Towong", "Wodonga", "Yackandandah", "Myrtleford",
  "Beechworth",
  "Wangaratta Region", "Wangaratta",
  "Yarrawonga", "Moira",
  "Cobram", "Numurkah", "Kyabram",
  "Shepparton Region - West", "Shepparton Region - East",
  "Shepparton - South", "Shepparton - North",
  "Mooroopna", "Nagambie", "Euroa",
  "Benalla", "Benalla Region",
  "Bright - Mount Beauty",
  "Chiltern - Indigo Valley", "West Wodonga")

abs_regions <- abs_regions[abs_regions$SA2_NAME16 %in% sa2_regions, ]


# transform to allow calculation of centroids and areas
abs_transformed <- st_transform(abs_regions, 3577) #st_crs = "+proj=longlat +datum=GDA94")
centroids <- st_centroid(abs_transformed)
centroids$area <- as.numeric(st_area(abs_regions))
# st_is_longlat(abs_regions) # should be FALSE

# ggplot(centroids) +
#   geom_sf(mapping = aes(color = log10(area)))

# convert back to long/lat
centroids_longlat <- st_transform(centroids, "+proj=longlat +datum=AGD84")

# worth excluding sites < particular size? These are urban so probably better ignored

ggplot(abs_regions[abs_regions$SA2_NAME16 == "Yass", ]) +
  geom_sf(fill = NA, color = "grey30") +
  theme_void()

# plot the whole region with the highlighted region in color
ggplot(abs_regions) +
  geom_sf(fill = "grey90", color = "grey30") +
  geom_sf(data = abs_regions[abs_regions$SA2_NAME16 == "Yass", ],
    color = NA,
    fill = "#36c275") +
  geom_sf(
    data = centroids_longlat,
    mapping = aes(color = cut(log10(centroids_longlat$area), breaks = c(0, 8.5, 20)))
  ) +
  ggtitle("ABS SA2 Regions", subtitle = "<p style='color:#36c275'>Region of Yass</p>") +
  theme_void() +
  theme(
    plot.subtitle = element_markdown(),
    legend.position = "none"
  )
# looks like the breaks are correct on this one


kept_centroids <- centroids_longlat[log10(centroids_longlat$area) > 8.5, ]
plot_data <- as.data.frame(do.call(rbind, lapply(
  strsplit(as.character(kept_centroids$geometry), ", "),
  function(a){as.numeric(gsub("c\\(|\\)", "", a))})))
colnames(plot_data) <- c("longitude", "latitude")
plot_data$label <- kept_centroids$SA2_NAME16
plot_data$state <- kept_centroids$STE_NAME16
plot_data$color <- c("#4e9c63", "#4e839c")[as.numeric(as.factor(plot_data$state))]
# saveRDS(plot_data, "./app/data/sa2_points.rds")
# ggplotly(ggplot(plot_data, aes(x = longitude, y = latitude, color = state)) +
#   geom_point() +
#   theme_void())

# extract climate data per region, add to plot data
## NOTE: This method extracts values at centroids.
## THIS IS NOT THE SAME AS GETTING THE AVERAGE VALUE ACROSS A POLYGON
## ergo some updating will be needed later

library(raster)
# plot_data <- readRDS("./app/data/sa2_points.rds")
# get a set of cordinates that fall within four tiles that our data could be in
lookup_coordinates <- data.frame(
  latitude = c(-28, -28, -34, -34),
  longitude = c(144, 152, 144, 152))
# get data and extract relevant points
worldclim_list <- lapply(
  split(lookup_coordinates, seq_len(4)),
  function(a){
    temp_data <- getData(
      name = "worldclim",
      download = TRUE,
      res = 0.5,
      var = "bio",
      lon = a$longitude,
      lat =  a$latitude)
    return(raster::extract(temp_data, plot_data[, 1:2]))
  })
# look up which list entry to get each datum from
row_index <- apply(
  do.call(cbind,
    lapply(worldclim_list, function(a){apply(a, 1, function(x){all(!is.na(x))})})),
  1,
  which)
# extract correct data for each row
result_df <- as.data.frame(do.call(rbind,
  lapply(seq_along(row_index), function(a){worldclim_list[[row_index[a]]][a, ]})
))[, c(1, 12, 7, 15, 18)]
colnames(result_df) <- c("AnnMeanTemp", "AnnPrec", "AnnTempRange", "PrecSeasonality", "PrecWarmQ")
# any(is.na(result_df)) # == FALSE
plot_data <- cbind(plot_data, result_df)
# delete downloaded files
unlink("wc0.5", recursive = TRUE)
# save
saveRDS(plot_data, "./app/data/sa2_points.rds")



# save polygons that we have kept
# NOTE: needs updating to exclude forested regions (outside of study envelope)
saveRDS(abs_regions[abs_regions$SA2_NAME16 %in% plot_data$label, ], "./app/data/sa2_polygons.rds")

# example map
plot_ly(
  plot_data,
  x = ~longitude,
  y = ~latitude,
  type = "scatter",
  mode = "markers",
  marker = list(
    size = 10,
    color = ~color
  ),
  hoverinfo = "text",
  text = ~label
) %>% layout(
  xaxis = list(nticks = 5),
  yaxis = list(scaleanchor = "x")
)

) %>%
  add_markers(
    x = ~longitude,
    y = ~latitude,
    color = ~state)

  colors = c("red", "blue")
)

# example climate plot
# unique(plot_data$color)
library(ggbeeswarm)
plot_data2 <- data.frame(
  label = rep(plot_data$label, 4),
  variable = rep(
    c("Annual Mean Temperature", "Annual Preciptiation",
      "Annual Temperature Range", "Precipitation Seasonality"),
    each = nrow(plot_data)),
  value = c(plot_data$AnnMeanTemp * 0.1, plot_data$AnnPrec,
    plot_data$AnnTempRange * 0.1, plot_data$PrecSeasonality))

ggplot(plot_data2, aes(x = value, y = 1, color = variable)) +
  facet_wrap(vars(variable), ncol = 2, scales = "free_x") +
  geom_quasirandom(
    data = plot_data2[plot_data$label != "Goulburn Region", ],
    size = 2, groupOnX = FALSE) +
  geom_quasirandom(
    data = plot_data2[plot_data$label == "Goulburn Region", ],
    size = 4, groupOnX = FALSE, color = "black") +
  theme_bw() +
  scale_color_manual(values = c("#4e839c", "#4e9c63", "#81a2b3", "#82b38f")) +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    # axis.line.y = element_blank(),
    panel.grid.minor.x = element_line(color = "grey70"),
    panel.grid.major.x = element_line(color = "grey70"),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.background = element_rect(fill = "grey90", colour = NA),
    panel.border = element_blank()
    # plot.background = element_rect(fill = "#d9d9d9", colour = NA)
  )
