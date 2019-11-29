#' @title Convert SWS Site Data in an SPDF
#' @description Reads in THE data frame of SWS site data and converts it into a Spatial Points Data Frame.
#' Projection information is not included yet.
#' @param swssite_dataframe A data frame of the SWS site data
#' @return An SpatialPointsDataFrame version of the SWS side data. Projection information is not included yet.
#' @importFrom sp SpatialPointsDataFrame
#' @importFrom sp CRS

#' @example 
#' sws_sites <- readRDS("./data/sws_sites.rds")
#' sws_sites_spdf <- swssites2spdf(sws_sites)


swssites2spdf <- function(swssite_dataframe){
  # remove all the sites that do not have a complete spatial location:
  sws_sites_cleaned <- swssite_dataframe[!(is.na(swssite_dataframe[,"longitude"]) | is.na(swssite_dataframe[,"latitude"])), ]
  sws_sites_cleaned <- sws_sites_cleaned[!(duplicated(sws_sites_cleaned$longitude) & duplicated(sws_sites_cleaned$latitude)), ]
  row.names(sws_sites_cleaned) <- sws_sites_cleaned$SiteCode
  sws_sites_spdf <- SpatialPointsDataFrame(sws_sites_cleaned[,c("longitude","latitude")], sws_sites_cleaned, proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  return(sws_sites_spdf)
}
