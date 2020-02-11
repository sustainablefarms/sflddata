##### Preparation #######

out <- lapply(c("sf", "tsibble", 'lubridate', "viridis",
                'ggplot2', 'tidyr', 'grid', 'gridExtra', 
                'feasts', 'dplyr', 'gtable', 'fable',
                'mgcv', "ggrepel", "sf"),
       library, character.only = TRUE)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

## Function to import site data:
read_tidy_pggpp_site_data <- function() {
  load("./private/data/remote_sensed/pg_daily.Rdata")
  pg_daily$times <- as_date(pg_daily$times)
  pg <- pg_daily %>%
    pivot_longer(-times, names_to = "site", values_to = "pg") %>%
    as_tsibble(key = site, index = times)
  load("./private/data/remote_sensed/gpp_8d.Rdata")
  gpp <- gpp_8d %>%
    pivot_longer(-times, names_to = "site", values_to = "gpp") %>%
    as_tsibble(key = site, index = times)
  pggpp <- as_tsibble(dplyr::full_join(pg, gpp, by = c("times", "site")), key = site, index = times)
  
  # add times breakdowns
  pggpp <- pggpp %>%
    mutate(yday = yday(times),
           year = year(times))
    
  
  #interpolate gpp
  pggpp <- pggpp %>%
    group_by_key() %>% #key is site
    mutate(lininterp_gpp = zoo::na.approx(gpp, rule = 2, na.rm = FALSE)) %>% #rule 2 means edges are assigned last value
    ungroup()
  
  #make sure sites are ordered alphabetically
  pggpp <- pggpp %>%
    arrange(site) %>%
    mutate(site = factor(site, ordered = TRUE))
  
  #separate alpha part of site code
  pggpp <- pggpp %>%
    mutate(farm = factor(substr(site, 1, 4)),
           sitenum = factor(as.integer(substr(site, 5, 5))))
  
  
  # Add cumulative rainfalls
  pggpp <- pggpp %>% 
    group_by_key() %>%
    mutate(pg_cumsum = cumsum(pg)) %>%
    mutate(pg_1to7 = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 8), # 1 up to 8 days behind (excluding 8th day)
           pg_1to15 = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 16), # 1 to 16 days behind
           pg_1to1m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 31),
           pg_1to2m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 2*31),
           pg_1to3m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 3*31),
           pg_1to4m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 4*31),
           pg_1to5m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 5*31),
           pg_1to6m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 6*31),
           pg_1to7m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 7*31),
           pg_1to8m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 8*31),
           pg_1to10m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 10*31),
           pg_1to12m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 12*31),
           pg_1to14m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 14*31),
           ) %>%
    mutate(pg_8d = pg_cumsum - lag(pg_cumsum, n = 8),
           #0 to day 7 (8 days) cumulative rainfall to correspond with gaps in GPP
           pg_24d = pg_cumsum - lag(pg_cumsum, n = 3*8)) %>% 
           # every 24 days (3*8) of GPP should be less correlated (given average GPP), this pg_24d corresponds to the rain through that period
    ungroup()
  
  # simple median of values
  ydaymedian <- pggpp %>%
    filter(yday %in% seq(1, 366, by = 8)) %>%
    group_by(site) %>%
    index_by(yday) %>%
    summarise(gpp.ydaymed = median(gpp),
              pg_8d.ydaymed = median(pg_8d),
              pg_24d.ydaymed = median(pg_24d),
              pg_1to5m.ydaymed = median(pg_1to5m, na.rm = TRUE))
  pggpp <- left_join(pggpp, ydaymedian, by = c("site", "yday"))
  return(pggpp)
}
pggpp <- read_tidy_pggpp_site_data()


filtertrain1 <- function(x){
  x <- x %>% filter(sitenum == 1,
                    !is.na(pg_1to14m),
                    yday %in% seq(1, 366, by = 3 * 8), #reduce correlation in data fitting and reduce fitting time
                    !is.na(gpp))
  return(x)
}


##### Plots of GPP prediction and Observed Longtitudinal #####

newdata <- pggpp %>%
  filtertrain1() %>%
  tibble::rownames_to_column(var = "rowname")
pred <- newdata %>%
  left_join(as_tibble(x = data.frame(predict(m1b,
                                             newdata = newdata,
                                             type = "response")), rownames = "rowname"),
            by = "rowname") %>% 
  rename(linpred = "predict.m1b..newdata...newdata..type....response..") %>%
  mutate(pred_m1b = inv_box_cox(linpred, lambda = 0.1414141) * gpp.ydaymed)
pred %>%
  filter(farm %in% c("PAEC")) %>%
  filter_index("2010" ~ "2015") %>%
  pivot_longer(c(pred_m1b, gpp, gpp.ydaymed), values_to = "GPP", names_to = "Type") %>%
  ggplot() +
  geom_line(aes(x = times, y = GPP, col = Type, lty = Type, size = Type), alpha = 0.8) +
  scale_color_viridis_d(labels = c("Observed", "Median GPP across years", "Predicted"),
                        end = 0.8,
                        name = NULL) +
  scale_size_manual(labels = c("Observed", "Median GPP across years", "Predicted"),
                    values = c(1.2, 0.5, 1) * 1.5,
                    name = NULL) +
  scale_linetype_manual(labels = c("Observed", "Median GPP across years", "Predicted"),
                        values = c("solid", "dotted", "dotdash"),
                        name = NULL) +
  ggtitle("GPP Model from Rainfall", subtitle = "Showing farm PAEC") +
  theme(legend.position="bottom")
ggsave("./private/plots/FebTeamMeet_GPP_Predictions_PAEC.png",
         width = 8, height = 6, units = "in")



#### Spatial Plots of Residual Summary Statistics ####
# prep contextual vector data:
swspoints <- sws_sites_2_sf(readRDS("./private/data/clean/sws_sites.rds"))
majorfeatures <- readRDS("./private/data/basemaps/GA_principalroads_majorrivers_railsways.rds")  %>% 
  st_transform(st_crs(swspoints)) %>%
  st_crop(swspoints)
builtupareas <- readRDS("./private/data/basemaps/GA_builtupareas.rds")  %>% 
  st_transform(st_crs(swspoints)) %>%
  st_crop(swspoints)

# in the below CANNOT use an aesethetic colour mapping as ggplot2 only allows one colour mapping and that is needed for the mean species richness in the following layers
contextlyrs <- list(
  geom_sf(data = majorfeatures %>% dplyr::filter(FEATTYPE %in% c("Major Road")),
          inherit.aes = FALSE, col = "grey"), 
  # geom_sf(data = majorfeatures %>% dplyr::filter(FEATTYPE %in% c("Major Watercourse")),
  #         inherit.aes = FALSE, lty = "dotted", col = "grey"),
  geom_sf(data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
          inherit.aes = FALSE, fill = "grey", col = "grey", lwd = 1),
  geom_text_repel(aes(x = cen.X, y = cen.Y,
                      label = NAME), #to title case: tools::toTitleCase(tolower(as.character(NAME)))),
                  data = builtupareas %>% dplyr::filter(SHAPE_Area > quantile(builtupareas$SHAPE_Area, 0.8)),
                  inherit.aes = FALSE,
                  nudge_y = 0.1,
                  col = "grey",
                  size = 3)
)


m1bresidsummaries <- readRDS("./private/data/derived/m1b_resids_summaries.rds")
sws_sites <- readRDS("./private/data/clean/sws_sites.rds")
sws_sites$farm <- factor(substr(sws_sites$SiteCode, 1, 4))
sws_sites$sitenum = factor(as.integer(substr(sws_sites$SiteCode, 6, 6)))
m1bresidsummaries <- sws_sites %>% 
  select("SiteCode", "farm", "sitenum", "longitude", "latitude") %>%
  unique() %>%
  right_join(m1bresidsummaries, by = "farm") %>%
  filter(sitenum == 1)


ggplot(m1bresidsummaries,
       aes( x = longitude, y = latitude, fill = q90, size = q90)
) +
  contextlyrs +
  geom_point(shape = 21) +
  scale_fill_viridis(direction = -1) +
  # scale_size(range = c(1, 6)) +
  theme_bw() +
  # coord_fixed() +  # removed by KH: clashses with geom_sf() - creates error
  guides(
    fill = guide_legend("Top 10% of Residuals"),
    size = guide_legend("Top 10% of Residuals")
  ) +
  ggtitle("Indication of Size of GPP Peaks that were Underestimated",
          subtitle = "Larger ==> More Unexpected GPP Amounts ==> Lower Grazing?") +
  xlab("Longitude") +
  ylab("Latitude")
ggsave("./private/plots/FebTeamMeet_m1b_resid_q90.png",
         width = 7, height = 6.42, units = "in")




# write.csv(m1bresidsummaries, file = "m1b_residsummaries.csv")
# library(eviatlas); library(shinydashboard)
# eviatlas()
