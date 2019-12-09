# explore daily time series data
library("tsibble"); library(lubridate); library(ggplot2); library(tidyr); library(gridExtra); library(feasts);

#load data and convert to tsibbles
load("./data/pg_daily.Rdata")
pg_daily$times <- as_date(pg_daily$times)
pg <- pg_daily %>%
  pivot_longer(-times, names_to = "site", values_to = "pg") %>%
  as_tsibble(key = site, index = times)

load("./data/gpp_8d.RData")
gpp <- gpp_8d %>%
  pivot_longer(-times, names_to = "site", values_to = "gpp") %>%
  as_tsibble(key = site, index = times)


pggpp <- as_tsibble(dplyr::full_join(pg, gpp, by = c("times", "site")), key = site, index = times)


# Inital plots of time series for a year
pgplot <- pggpp %>%
  dplyr::filter(site == "BELL1") %>%
  tsibble::filter_index("2001-01-01" ~ "2001-12-31") %>%
  ggplot() +
  geom_line(aes(times, pg))

gppplot <- gpp %>%
  dplyr::filter(site == "BELL1") %>%
  ggplot() +
  geom_line(aes(times, gpp))

grid.draw(rbind(ggplotGrob(pgplot), ggplotGrob(gppplot), size = "last"))

# Two locations in the Same Plot
# Inital plots of time series for a year
pgplot <- pggpp %>%
  dplyr::filter(site %in% c("ARCH1", "ARCH2")) %>%
  tsibble::filter_index("2001-01-01" ~ "2001-12-31") %>%
  ggplot() +
  geom_line(aes(times, pg)) + 
  facet_grid(. ~ site)

gppplot <- gpp %>%
  dplyr::filter(site %in% c("ARCH1", "ARCH2")) %>%
  tsibble::filter_index("2001-01-01" ~ "2001-12-31") %>%
  ggplot() +
  geom_line(aes(times, gpp)) +
  facet_grid(. ~ site)

grid.draw(rbind(ggplotGrob(pgplot), ggplotGrob(gppplot), size = "last"))

# Plot GPP for one year for all locations
gpp %>%
  tsibble::filter_index("2001-01-01" ~ "2001-12-31") %>%
  ggplot() +
  geom_line(aes(times, gpp, color = site)) +
  guides(color = "none")


#### Plots that don't work ####
# Seasonal plots
pgplot <- pggpp %>%
  dplyr::filter(site == "BELL1") %>%
  feasts::gg_season(pg)

# Monthly aggregates
pgplot <- pggpp %>%
  dplyr::filter(site == "BELL1") %>%
  index_by(year_month = ~ yearmonth(.)) %>%
  feasts::autoplot(pg)


gppplot <- pggpp %>%
  dplyr::filter(site == "BELL1") %>%
  group_by_key(year(times)) %>%
  feasts::gg_subseries(gpp, period = years())
gppplot

pggpp$year <- year(pggpp$times)
pggpp$times - pggpp$year
gppplot <- pggpp %>%
  dplyr::filter(site == "BELL1") %>%
  group_by_key(year) %>%
  feasts::autoplot()
gppplot


gpp %>%
  dplyr::filter(site %in% c("ARCH1", "BELL1")) %>%
  ggplot() +
  geom_line(aes(times, gpp)) + 
  facet_grid(site ~ .)
