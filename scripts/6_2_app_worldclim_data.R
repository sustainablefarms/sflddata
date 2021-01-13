sa2_points <- readRDS("../../farm_biodiversity_app/data/sa2_points.rds")
sa2_points <- sa2_points[, c("longitude", "latitude", "label", "state", "color")]

# Get worldclim data at locations
# NOTE: This script will drop a file called wc0.5 in your home directory
  # you can delete it when done

# get location for worldclim data
library(raster)

# get a set of cordinates that fall within four tiles that our data could be in
lookup_coordinates <- data.frame(
  latitude = c(-28, -28, -34, -34),
  longitude = c(144, 152, 144, 152))

# get data and extract relevant points
worldclim_list <- lapply(
  split(lookup_coordinates, seq_len(4)),
  function(a){
    temp_data <- raster::getData(
      name = "worldclim",
      download = TRUE,
      res = 0.5,
      var = "bio",
      lon = a$longitude,
      lat =  a$latitude)
    return(extract(temp_data, sa2_points[, c("longitude", "latitude")]))
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
))
# any(is.na(result_df)) # == FALSE

# put nice names of data
climnames <- readRDS("../private/data/clean/7_2_10_climate_names_table.rds")
namemap <- tibble::deframe(climnames[, c("code", "shortname")])
colnames(result_df) <- namemap[gsub("_.*", "", colnames(result_df))]

# cut down to only data that the model needs
result_df <- result_df[, c("AnnPrec", "MaxTWarmMonth", "MinTColdMonth", "PrecSeasonality")]

result_df <- cbind(sa2_points, result_df)


saveRDS(result_df, "../../farm_biodiversity_app/data/sa2_points_climate.rds")
