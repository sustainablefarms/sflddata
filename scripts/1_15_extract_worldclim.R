# Get worldclim data at site locations
# NOTE: This script will drop a file called wc0.5 in your home directory
  # you can delete it when done

# get site locations
locations <- read.csv("./private/data/clean/site_locations_cleaned.csv")[, c(1, 10, 11)]
colnames(locations) <- c("site_code", "longitude", "latitude")

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
    return(extract(temp_data, locations[, 2:3]))
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

result_df <- cbind(locations, result_df)
write.csv(result_df, "./private/data/clean/worldclim_data.csv", row.names = FALSE)