#' @title Map Albert's Remotely Sensed Data Drills to Three Post Sites
#' @param valabbrv Abbreviation of value name used in Albert's file naming convention.
#' @param timecode The time code (typically year) that is used in Albert's files.
#' @param threepostsfile The csv file of post locations used by Albert. 
#' This file contains the site and study information in the same order as the remote sensed values.
#' @return A tibble with columns Study SiteCode post, LON, LAT, 
#' and data columns named by valabbrv_timecode_radius (e.g. maxBS_2000_1000)
#' @examples 
#' out <- getsite_fromonetwothreeposts("maxBS", 2001, "./AlbertData/threeposts_wgs84.csv")
#' @export
getsite_fromonetwothreeposts <- function(valabbrv, timecode, threepostsfile = "./AlbertData/threeposts_wgs84.csv"){
  datadir <- dirname(threepostsfile)
  rsfilename <- paste0(datadir, "/", valabbrv, timecode, "_onetwothreeposts.csv")

  # reading method: match to the list of posts
  # read in post info
  threeposts <- read.csv(threepostsfile)
  threeposts$LONav <- rowMeans(threeposts[, c("LON0m", "LON100m", "LON200m")], na.rm = FALSE)
  threeposts$LATav <- rowMeans(threeposts[, c("LAT0m", "LAT100m", "LAT200m")], na.rm = FALSE)

  # note that every site has a 0m post. 52 sites do not have 100m and 200m posts, and 2 sites are missing only the 100m post
  # threeposts %>%  dplyr::filter(is.na(LONav)) %>% View()

#making threeposts compatible with the rows of Albert's data
threeposts_compatible <- threeposts %>%  
  tidyr::pivot_longer(
    cols = starts_with("L"),
    names_to = c(".value", "post"),
    names_pattern = "(...)(.*)",
    values_to = "value") %>%
  dplyr::mutate(posttype = 30 + dplyr::case_when(
    post == "0m" ~ 1,
    post == "100m" ~ 2,
    post == "200m" ~ 3,
    post == "av" ~ 0
  )) %>%
  dplyr::arrange(post) %>%
  dplyr::filter(!is.na(LAT), !is.na(LON))

  # read in RS file
  rsdata <- read.csv(rsfilename, check.names = TRUE)
  colnames(rsdata)[1:3] <- c("posttype", "latitude", "longitude")
  rsdata <- rsdata[rsdata$posttype %in% c(31, 32, 33, 30), ]

  # combine with the site labels for the posts
  rssitedata <- cbind(threeposts_compatible, rsdata)
  # check distances within about 10m
  stopifnot(max(abs(rssitedata$LON - rssitedata$longitude)) < 1E-04)
  stopifnot(max(abs(rssitedata$LAT - rssitedata$latitude)) < 1E-04) 
  # clean up table
  rssitedata <- rssitedata %>% dplyr::select(!c(posttype, latitude, longitude)) # remove extra columns
  rssitedata <- replace(rssitedata, rssitedata == -9999.0, NA)

  # for each site use the value of the average post values for > 25m
  rssite_dataselected <- rssitedata %>%
    dplyr::filter(post != "100m") %>%
    dplyr::group_by(SiteCode, Study) %>%
    dplyr::filter(post == last(post)) %>%
    dplyr::select(-post, -X25) %>%
    ungroup()
  
  # for each site use the average of the post values (for 25m, Lat and Lon)
  rssite_av <- rssitedata %>%
    dplyr::group_by(SiteCode, Study) %>%
    dplyr::filter(post != "av") %>%
    dplyr::select(SiteCode, Study, LON, LAT, X25) %>%
    dplyr::summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    ungroup()
  
  rssite_final <- inner_join(rssite_av, rssite_dataselected, by = c("Study", "SiteCode"))
  stopifnot(0.99 < mean(rssite_final$LON.x == rssite_final$LON.y))
  stopifnot(0.99 < mean(rssite_final$LAT.x == rssite_final$LAT.y))
  rssite_final$LON.y <- NULL
  rssite_final$LAT.y <- NULL
  colnames(rssite_final)[c(3, 4)] <- c("LON", "LAT")

  # 13 sites do not have any: 3 in BBMP, 1 in Nanangroe, and 8 in stewardship
  if (13 != sum(!complete.cases(rssite_final))){
    warning("There is usually 13 sites without data, but this data set is different.")
  }

# rename columns
  colnames(rssite_final) <- gsub("^X", paste0(valabbrv, "_", timecode, "_"), colnames(rssite_final))
  return(rssite_final)
}
