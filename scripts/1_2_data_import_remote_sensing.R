# script to import and clean remote sensing data
# Note: this requires functions stored in "./functions/csv_import.R"

# THIS SCRIPT IS OUT OF DATE
# KASS IS CURRENTLY IN PROCESS OF MOVING TO DIRECT QUERIES OF THE WALD DATABASE
# ONCE THAT WORKFLOW IS COMPLETE THIS FILE CAN BE REMOVED

# Gross Primary Productivity
gpp_values <- get_observations("./data/Birdsite_GPP_FMC_pointdrills_GPP.csv")
gpp_values$site <- sub("\\.", "-", gpp_values$site)
colnames(gpp_values)[3] <- "gpp"

# Fuel Moisture Content
fmc_values <- get_observations("./data/Birdsite_GPP_FMC_pointdrills_FMC.csv")
fmc_values$site <- sub("\\.", "-", fmc_values$site)
colnames(fmc_values)[3] <- "fmc"

# any(is.na(fmc_values$site))
# any(is.na(gpp_values$site)) # checks

# import site data
sites <- readRDS("./data/sws_sites.rds")


# # try merging by site and date
# test_sites <- merge(sites, gpp_values,
#   by.x = c("SiteCode", "SurveyDate"),
#   by.y = c("site", "date"),
#   all.x = TRUE,
#   all.y = FALSE
# )
# fails, apparently because the dates in rs data do not exactly match those in the monitoring data
# ergo we need to match to the closest possible date

# set up locations in site data to add new variables
sites$date <- lubridate::ymd(sites$SurveyDate)
sites$gpp <- NA
sites$fmc <- NA
site_list <- split(sites, seq_len(nrow(sites)))

# use lapply to find the nearest observation of each variable
# to an observation in our dataset
# a <- site_list[[2]] # testing
site_list2 <- lapply(site_list, function(a, gpp, fmc){
  # gpp
  site_test <- gpp$site == a$SiteCode
  if(any(site_test)){
    rows1 <- which(site_test)
    row <- rows1[which.min(as.numeric(gpp$date[rows1] - a$date)^2)]
    a$gpp <- gpp$gpp[row]
  }
  # fmc
  site_test <- gpp$site == a$SiteCode
  if(any(site_test)){
    rows1 <- which(site_test)
    row <- rows1[which.min(as.numeric(fmc$date[rows1] - a$date)^2)]
    a$fmc <- fmc$fmc[row]
  }
  return(a)
},
fmc = fmc_values,
gpp = gpp_values
)

sites_rs <- do.call(rbind, site_list2)
# sites_rs$year <- as.numeric(sites_rs$date)
# length(which(is.na(sites_rs$gpp)))