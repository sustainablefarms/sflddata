#' @title Functions to acquire data for model predictors that is available online.
#' @param coords An [sf] object with coordinates.
#' @param years Years of woody cover to use.
#' @param model A character code providing the model for which data to acquire

#' @examples 
#' locations <- read.csv("./private/data/clean/site_locations_cleaned.csv")
#' locs <- sf::st_as_sf(locations, coords = c("MeanLON", "MeanLAT"), crs = 4326)
#' coords <- locs[ locs$StudyCode == "Nanangroe Natural Experiment", ]
#' fromwebdata <- ll2webdata(coords, 2018)

ll2webdata <- function(coords, years, model = "7_4"){
  stopifnot(model == "7_4")
  stopifnot(sf::st_crs(coords) == sf::st_crs(4326))
  worldclimdata <- ll2worldclim(coords)
  woody500m <- ll2woodybuffer(coords, years, buffer = 500)
  colnames(woody500m) <- gsub("X", "w500m", colnames(woody500m))
  return(cbind(worldclimdata, woody500m))
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




#### Woody500m ####
#' @param buffer is radius to average woody vegetation in __metres__.
#' @param epsgrect The numerical ID of a coordinate reference system to temporarily use for rectangular operations like distances. 
#' Must have units of metres.
#' Default of 8058 is the GDA2020 / NSW Lambert coordinate system.
ll2woodybuffer <- function(coords, years, buffer = 500, maxattempts = 5, epsgrect = 8058){
  stopifnot(!is.null(years))
  stopifnot(sf::st_crs(coords) == sf::st_crs(4326))
  coords$tmmmmmmmmpid <- 1:nrow(coords)
  
  ## divide locations into tiles for raster extraction, convert to and from GDA2020 / NSW Lambert for rectangular calculations
  # cellbuffer <- 2 * buffer / 100 * 1000 #for cell buffer, double the buffer for the woody veg and convert into approximate degrees
  # cellsize <- 5 * cellbuffer
  # tileswpts <- divide_into_tiles(coords, cellsize = cellsize, buffer = cellbuffer)
  ncoords <- sf::st_transform(coords, crs = epsgrect) #GDA2020 / NSW Lambert
  tileswpts <- divide_into_tiles(ncoords, cellsize = 5 * 2 * buffer, buffer = 2 * buffer)
  tileswpts <- lapply(tileswpts, function(x) list(tile = sf::st_transform(x$tile, 4326),
                                     pts = sf::st_transform(x$pts, 4326)))
  
  # purely for plotting tiles:
  # tiles <- lapply(tileswpts, function(x) st_as_sf(x$tile))
  # tilegrd <- do.call(rbind, tiles)
  # plot(tilegrd)
  
  ## read RS rasters, multiple attempts
  woodyl <- lapply(tileswpts, function(x) return(NULL))
  uncompleted <- vapply(woodyl, is.null, FUN.VALUE = FALSE)
  attempts <- 0
  while(any(uncompleted) && attempts <= maxattempts){
    cat("Attempting:", sum(uncompleted), "tiles.\n")
    woodyl[uncompleted] <- pbapply::pblapply(tileswpts[uncompleted],
                                             FUN = function(x) {
                                               woody <- NULL
                                               try(woody <- suppressDatumDiscardWarning(woody_vals_buffer(x$tile, x$pts, years, 500)))
                                               gc()
                                               return(woody)
                                             })
    uncompleted <- vapply(woodyl, is.null, FUN.VALUE = FALSE)
    attempts <- attempts + 1
  }
  
  # combine the results back into order
  woodyl_tmpid <- lapply(1:length(woodyl),
                             function(id){
                               woodyvals <- woodyl[[id]]
                               pts <- tileswpts[[id]]$pts
                               woodyvals$tmmmmmmmmpid <- pts$tmmmmmmmmpid
                               return(woodyvals)
                             })
  woody <- do.call(rbind, woodyl_tmpid)
  woody <- woody[order(woody$tmmmmmmmmpid), colnames(woody) != "tmmmmmmmmpid", drop = FALSE]
  
  return(woody)
}
