# script to import and clean remote sensing data

# import site data
sites <- readRDS("./private/data/clean/sws_sites.rds")
sites$row_index <- seq_len(nrow(sites))
sites$SiteCode <- gsub("-", "", sites$SiteCode) # easier to match to rs data

# get input data
load("./private/data/remote_sensed/gpp_8d_tmn.Rdata") # gpp
load("./private/data/remote_sensed/gpp_8d_difftotmn.Rdata")
load("./private/data/remote_sensed/fmc_mean_tmn.Rdata") # fmc
load("./private/data/remote_sensed/fmc_mean_difftotmn.Rdata")
load("./private/data/remote_sensed/woodycover_500mradius.Rdata") # woody cover
m1b_resid <- readRDS("./private/data/derived/m1b_resid.rds") #residuals of GPP predictions
rm(session) # loaded with .RData but not required
# attributes:
  # gpp_8d_tmn, fmc_mean_tmn: 176 sites, names stored in rownames
  # gpp_8d_difftotmn, fmc_mean_difftotmn: 874|1737 rows (dates), 177 columns (date + sites)


# convert mean values to data.frames
gpp_mean <- data.frame(
  site = names(gpp_8d_tmn),
  gpp_mean = gpp_8d_tmn,
  stringsAsFactors = FALSE
)
fmc_mean <- data.frame(
  site = names(fmc_mean_tmn),
  fmc_mean = fmc_mean_tmn,
  stringsAsFactors = FALSE
)

# convert m1b residuals to long format
library(tidyr); library(dplyr)
m1b_resid <- m1b_resid %>%
  tidyr::pivot_longer(-times, names_to = "site", values_to = "m1b_resid") %>%
  dplyr::rename(date = times)

# convert gpp_diff to long format
colnames(gpp_8d_difftotmn)[1] <- "date"
gpp_diff_list <- lapply(
  colnames(gpp_8d_difftotmn)[-1],
  function(a, data){
    data.frame(
      site = a,
      date = data[, 1],
      gpp_diff = data[, a],
      stringsAsFactors = FALSE
    )
  }, data = gpp_8d_difftotmn
)
gpp_diff <- do.call(rbind, gpp_diff_list)
rm(gpp_diff_list)

# ditto fmc
colnames(fmc_mean_difftotmn)[1] <- "date"
fmc_diff_list <- lapply(
  colnames(fmc_mean_difftotmn)[-1],
  function(a, data){
    data.frame(
      site = a,
      date = data[, 1],
      fmc_diff = data[, a],
      stringsAsFactors = FALSE
    )
  }, data = fmc_mean_difftotmn
)
fmc_diff <- do.call(rbind, fmc_diff_list)
rm(fmc_diff_list)

# ditto woody_cover
woody_cover_list <- lapply(
  colnames(woodycover_500mradius)[-1],
  function(a, data){
    data.frame(
      site = a,
      year = data[, 1],
      woody_cover = data[, a],
      stringsAsFactors = FALSE
    )
  }, data = woodycover_500mradius
)
woody_cover <- do.call(rbind, woody_cover_list)
rm(woody_cover_list)


# add mean gpp to site using merge
sites <- merge(sites, gpp_mean,
  by.x = "SiteCode", by.y = "site",
  all.x = TRUE, all.y = FALSE
)
# ditto mean fmc
sites <- merge(sites, fmc_mean,
  by.x = "SiteCode", by.y = "site",
  all.x = TRUE, all.y = FALSE
)


# for time-varying parameters, set up locations in site data to add new variables
sites$date <- lubridate::ymd(sites$SurveyDate)
sites$gpp_diff <- NA
sites$m1b_resid <- NA
sites$fmc_diff <- NA
sites$woody_cover <- NA
site_list <- split(sites, seq_len(nrow(sites)))

# use lapply to find the nearest observation of each variable
# to an observation in our dataset
# NOTE: An alternative is to use the closest observation to occur before our survey
# but no great need for that at present
# a <- site_list[[2]] # testing
site_list2 <- lapply(site_list, function(a, gpp, m1b_resid, fmc, woody_cover){
  # gpp
  site_test <- gpp$site == a$SiteCode
  if(any(site_test)){
    rows1 <- which(site_test)
    row <- rows1[which.min(as.numeric(gpp$date[rows1] - a$date)^2)]
    a$gpp_diff <- gpp$gpp[row]
  }
  # m1b_resid
  site_test <- m1b_resid$site == a$SiteCode
  if(any(site_test)){
    rows1 <- which(site_test)
    row <- rows1[which.min(as.numeric(m1b_resid$date[rows1] - a$date)^2)]
    a$m1b_resid <- m1b_resid$m1b_resid[row]
  }
  # fmc
  site_test <- gpp$site == a$SiteCode
  if(any(site_test)){
    rows1 <- which(site_test)
    row <- rows1[which.min(as.numeric(fmc$date[rows1] - a$date)^2)]
    a$fmc_diff <- fmc$fmc[row]
  }
  site_test <- woody_cover$site == a$SiteCode
  if(any(site_test)){
    rows1 <- which(site_test)
    rows2 <- rows1[woody_cover$year[rows1] <= a$SurveyYear]
    row <- rows2[which.min(as.numeric(woody_cover$year[rows2] - a$SurveyYear)^2)]
    a$woody_cover <- woody_cover$woody_cover[row]
  }
  return(a)
},
fmc = fmc_diff,
gpp = gpp_diff,
m1b_resid = m1b_resid,
woody_cover = woody_cover
)
sites_rs <- do.call(rbind, site_list2)
# sites_rs$year <- as.numeric(sites_rs$date)
# length(which(is.na(sites_rs$gpp)))

sites_rs <- sites_rs[
  order(sites_rs$row_index),
  which(colnames(sites_rs) != "row_index")]
saveRDS(sites_rs, "./private/data/clean/sws_sites_rs.rds")