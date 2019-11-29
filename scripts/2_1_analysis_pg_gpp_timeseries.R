# explore daily time series data
library("tsibble"); library(lubridate); library(ggplot2); library(tidyr);

load("pg_daily.Rdata")
pg_daily$times <- as_date(pg_daily$times)
pg_daily_long <- pg_daily %>% pivot_longer(-times, names_to = "site", values_to ="precipitation")

pg_daily <- as_tsibble(pg_daily_long, key = site, index = times)
ggplot(pg_daily) + geom_line(aes(times, precipitation))
#pg_daily %>% gg_season()
pg_daily %>%
  group_by_key(site) %>%
  slice(1)
