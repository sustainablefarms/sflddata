#' @title Convert SWS Site Data in an SPDF
#' @description Reads in THE data frame of SWS site data and converts it into a Spatial Points Data Frame.
#' Projection information is not included yet.
#' @param swssite_dataframe A data frame of the SWS site data
#' @return An SpatialPointsDataFrame version of the SWS side data. Projection information is not included yet.
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp CRS
#' @importFrom sf st_as_sf

#' @examples 
#' sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
#' sws_sites_spdf <- sws_sites_2_spdf(sws_sites)
#' sws_sites_sf <- sws_sites_2_sf(sws_sites)

#' @export
sws_sites_2_spdf <- function(swssite_dataframe){
  # remove all the sites that do not have a complete spatial location:
  sws_sites_cleaned <- swssite_dataframe[!(is.na(swssite_dataframe[,"longitude"]) | is.na(swssite_dataframe[,"latitude"])), ]
  sws_sites_cleaned <- sws_sites_cleaned[!(duplicated(sws_sites_cleaned$longitude) & duplicated(sws_sites_cleaned$latitude)), ]
  sws_sites_cleaned$SiteCode <- gsub("-","", sws_sites_cleaned$SiteCode) # remove dashes because it stuffs up variable calling in R sometimes
  row.names(sws_sites_cleaned) <- sws_sites_cleaned$SiteCode
  sws_sites_spdf <- SpatialPointsDataFrame(sws_sites_cleaned[,c("longitude","latitude")], sws_sites_cleaned, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  return(sws_sites_spdf)
}

#' @describeIn sws_sites_2_spdf Convert SWS Site Data in SF Object
#' @export
sws_sites_2_sf <- function(swssite_dataframe){
  # remove all the sites that do not have a complete spatial location:
  sws_sites_cleaned <- swssite_dataframe[!(is.na(swssite_dataframe[,"longitude"]) | is.na(swssite_dataframe[,"latitude"])), ]
  sws_sites_cleaned <- sws_sites_cleaned[!(duplicated(sws_sites_cleaned$longitude) & duplicated(sws_sites_cleaned$latitude)), ]
  sws_sites_cleaned$SiteCode <- gsub("-","", sws_sites_cleaned$SiteCode) # remove dashes because it stuffs up variable calling in R sometimes
  row.names(sws_sites_cleaned) <- sws_sites_cleaned$SiteCode
  sws_sites_sf <- sf::st_as_sf(sws_sites_cleaned, coords = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
  return(sws_sites_sf)
}
