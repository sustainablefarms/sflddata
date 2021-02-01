sa2_points <- readRDS("../../farm_biodiversity_app/data/sa2_points.rds")
sa2_points <- sa2_points[, c("longitude", "latitude", "label", "state", "color")]

# Get worldclim data at locations
# NOTE: This script will drop a file called wc0.5 in your home directory
  # you can delete it when done

locs <- sf::st_as_sf(sa2_points, coords = c("longitude", "latitude"), crs = 4326)
result_df <- sflddata::ll2worldclim(locs)


# cut down to only data that the model needs
result_df <- result_df[, c("AnnPrec", "MaxTWarmMonth", "MinTColdMonth", "PrecSeasonality")]

result_df <- cbind(sa2_points, result_df)

saveRDS(result_df, "../../farm_biodiversity_app/data/sa2_points_climate.rds")
