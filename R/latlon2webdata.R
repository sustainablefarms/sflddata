#' @title Functions to acquire data for model predictors that is available online.
#' @param coords An [sf] object with coordinates.
#' @param years Years of woody cover to use.
#' @param model A character code providing the model for which data to acquire

#' @examples 
#' locations <- read.csv("./private/data/clean/site_locations_cleaned.csv")
#' locs <- sf::st_as_sf(locations, coords = c("MeanLON", "MeanLAT"), crs = 4326)
#' coords <- locs[ locs$StudyCode == "Nanangroe Natural Experiment", ]

ll2webdata <- function(coords, years, model = "7_4"){
  stopifnot(sf::st_crs(coords) == sf::st_crs(4326))
  
}