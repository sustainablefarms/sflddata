## Preparation
out <- lapply(c("tsibble", 'tidyr', 'dplyr', 'fable'),  #fable used for box_cox transformation fcnt
       library, character.only = TRUE)
out <- lapply(paste0("./functions/", list.files("./functions/")), source)

m1b <- readRDS(file = "./private/models/m1b.rds")

### load gpp and pg data, and convert to tsibbles
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

### add time breakdowns
pggpp <- pggpp %>%
  mutate(yday = yday(times),
         year = year(times))
  
### make sure sites are ordered alphabetically
pggpp <- pggpp %>%
  arrange(site) %>%
  mutate(site = factor(site, ordered = TRUE))

### separate alpha part of site code
pggpp <- pggpp %>%
  mutate(farm = factor(substr(site, 1, 4)),
         sitenum = factor(as.integer(substr(site, 5, 5))))


### Add cumulative rainfalls
pggpp <- pggpp %>% 
  group_by_key() %>%
  mutate(pg_cumsum = cumsum(pg)) %>%
  mutate(pg_1to5m = lag(pg_cumsum, n = 1) - lag(pg_cumsum, n = 5*31)) %>%
  mutate(pg_24d = pg_cumsum - lag(pg_cumsum, n = 3*8)) %>% 
         # every 24 days (3*8) of GPP should be less correlated (given average GPP), this pg_24d corresponds to the rain through that period
  ungroup()


### simple median of values
ydaymedian <- pggpp %>%
  filter(yday %in% seq(1, 366, by = 8)) %>%
  group_by(site) %>%
  index_by(yday) %>%
  summarise(gpp.ydaymed = median(gpp),
            pg_24d.ydaymed = median(pg_24d),
            pg_1to5m.ydaymed = median(pg_1to5m, na.rm = TRUE))
pggpp <- left_join(pggpp, ydaymedian, by = c("site", "yday"))


## Make predictions using m1b:
pggpp$linpred <- predict.lm(m1b, newdata = pggpp)
m1bout <- pggpp %>%
  filter(yday(times) %in% seq(1, 366, by = 8)) %>%
  mutate(gpp.pred = inv_box_cox(linpred, lambda = 0.1414141) * gpp.ydaymed) %>%
  mutate(.resid = gpp - gpp.pred) %>%
  dplyr::select(times, site, gpp.pred, .resid)


m1b_pred <- m1bout %>%
  filter(yday(times) %in% seq(1, 366, by = 8)) %>%
  pivot_wider(id_cols = times, names_from = site, values_from = gpp.pred) %>%
  as.data.frame()

m1b_resid <- m1bout %>%
  pivot_wider(id_cols = times, names_from = site, values_from = .resid) %>%
  as.data.frame()

## Save Predictions and Residual
saveRDS(m1b_pred, "./private/data/derived/m1b_pred.rds")
saveRDS(m1b_resid, "./private/data/derived/m1b_resid.rds")

# inner_join(m1bout, pggpp) %>%
#   filter(site == "ARCH1") %>%
#   mutate(yday = yday(times)) %>%
#   filter(yday %in% seq(1, 366, by = 8)) %>%
#   ggplot() +
#   geom_line(aes(x = times, y = gpp.pred)) +
#   geom_point(aes(x = times, y = gpp), col = "blue")

