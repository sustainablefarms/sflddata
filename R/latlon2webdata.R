#' @title Functions to acquire data for model predictors that is available online.
#' @param coords An [sf] object with coordinates.
#' @param years Years of woody cover to use.
#' @param model A character code providing the model for which data to acquire

#' @examples 
#' locations <- read.csv("./private/data/clean/site_locations_cleaned.csv")
#' locs <- sf::st_as_sf(locations, coords = c("MeanLON", "MeanLAT"), crs = 4326)
#' coords <- locs[ locs$StudyCode == "Nanangroe Natural Experiment", ]
#' ll2webdata(coords)

ll2webdata <- function(coords, years, model = "7_4"){
  stopifnot(sf::st_crs(coords) == sf::st_crs(4326))
  worldclimdata <- ll2worldclim(coords)
  return(worldclimdata)
}

#### WorldClim #####
ll2worldclim <- function(coords){
  stopifnot(sf::st_crs(coords) == sf::st_crs(4326))
  ll <- sf::st_coordinates(coords)

  # prepare rasters of worldclim data, divided into 4, and extract values
  # get a set of cordinates that fall within four tiles that our data could be in
  tile_coordinates <- data.frame(
    latitude = c(-28, -28, -34, -34),
    longitude = c(144, 152, 144, 152))
  
  # get data and extract relevant points
  worldclim_list <- lapply(
    split(tile_coordinates, seq_len(4)),
    function(a){
      temp_raster <- raster::getData(
        name = "worldclim",
        download = TRUE,
        res = 0.5,
        var = "bio",
        lon = a$longitude,
        lat =  a$latitude)
      return(raster::extract(temp_raster, ll))
    })

  ## merge extractions into single dataframe
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
  stopifnot(nrow(coords) == nrow(result_df))
  rowallna <- apply(result_df, MARGIN = 1, function(x) all(is.na(x)))
  if (any(rowallna)){
    warning("Did not aquire data for locations ", which(rowallna))
  }
  # check the results for NA values
  cellna <- is.na(result_df[!rowallna, ])
  dimnames(cellna) <- dimnames(result_df)
  if (any(cellna)){
    warning("Some cells had NA values")
  }
  
  ## rename the columns to match the names used elsewhere
  colnames(result_df) <- gsub("_410$", "", colnames(result_df))   # remove the 410 suffix
  climnames <- readRDS(system.file("climate_names_table.rds", package = "sustfarmld"))
  code2shortname <- climnames$shortname
  names(code2shortname) <- climnames$code
  colnames(result_df) <- code2shortname[colnames(result_df)]
  return(result_df)
}


