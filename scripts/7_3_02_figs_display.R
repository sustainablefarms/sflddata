#### Preparation ####

library(tibble)
library(dplyr)
library(MCMCpack)
library(mclust)
library(corrplot)
library(coda)
library(runjags)
library(ggplot2)
library(patchwork)

inputdata <- readRDS("./private/data/clean/7_2_10_input_data.rds")

waics_l <- c(readRDS("./tmpdata/7_2_10_waics.rds"),
             readRDS("./tmpdata/7_2_11_waics.rds"),
             readRDS("./tmpdata/7_2_12_waics.rds"),
             readRDS("./tmpdata/7_2_13_waics.rds"),
             readRDS("./tmpdata/7_3_00_waics.rds")
             )

stopifnot(setequal(names(lpds_l), names(waics_l)))

# remove non-convergent MCMC models
unconverged <- c("twiwoody500mgppdiff_gppmean" , "twiwoody500mgppdiffgppmean", "twiwoody500mgppdiff",
                 "latlon", "latlon_year")
targetmods <- matrix(c(
  "interceptsonly",    "No Predictors",
  "moreclimate_year",    "Many More Climate Variables",
  "someclimate_year",    "Mean, Arid, Cold, and Stability Hypotheses",
  "MinT_PrecColdQ",    "Cold Limit Hypothesis",
  "AnnTempR_PrecCoV",    "Mean Climate Hypothesis",
  "os_msnm_gc",    "Overstorey, Ground Cover and\nNoisy Minor Interacting with Midstorey",
  "msnm",    "Noisy Minor Interacting with Midstorey",
  "nm",    "Only Noisy Minor",
  "twiwoody500m",    "Terrain Wetness and Nearby Tree Canopy",
  "woody500m",    "Nearby Tree Canopy",
  "gppmean",    "Mean Gross Primary Productivity",
  "someclimate_year_woody500m_msnm", "Mean, Arid, Cold, and Stability Hypotheses,\nNearby Tree Canopy, Noisy Minor Interacting with Midstorey"
  ),
  ncol = 2, byrow = TRUE)
colnames(targetmods) <- c("shortname", "longname")
waics_l <- waics_l[targetmods[, "shortname"]]
Enums_holdout <- readRDS("./tmpdata/7_3_01b_many_Enum_holdout.rds")[names(lpds_l)]
Enums_insample <- readRDS("./tmpdata/7_3_01b_many_Enum_insample.rds")[names(lpds_l)]

rs <- setdiff(names(readRDS("./tmpdata/7_2_13_lpds.rds")), "interceptsonly")
grnd <- names(readRDS("./tmpdata/7_2_11_lpds.rds"))
climate <-  c(names(readRDS("./tmpdata/7_2_10_lpds.rds")), names(readRDS("./tmpdata/7_2_12_lpds.rds")))

modelname2type <- function(modelnames){
  modeltype <- case_when(
    modelnames %in% c("interceptsonly", "intercepts_only") ~ "",
    modelnames %in% rs ~ "Remote",
    modelnames %in% climate ~ "Climate",
    modelnames %in% grnd ~ "Ground",
    TRUE ~ "Combined"
    )
  modeltype <- factor(modeltype, levels = c("Climate", "Ground", "Remote", "Combined", "", "Other"), ordered = TRUE)
  return(modeltype)
}

varname2type <- function(varnames){
  types <- case_when(
    grepl("lv.coef", varnames) ~ "LV Load",
    grepl("LV", varnames) ~ "LV",
    grepl("^(mu|tau)", varnames) ~ "Comm Param", #parameters of community distributions
    grepl("^u.b", varnames) ~ "Occu Coef",
    grepl("^v.b", varnames) ~ "Detn Coef",
    TRUE ~ "other"
    )
  return(types)
}

plot_compare_loo <- function(compare.loo.obj){
  plt <- compare.loo.obj %>%
    as_tibble(rownames = "Model") %>%
    dplyr::mutate(modeltype = modelname2type(Model)) %>%
    ggplot() +
    geom_errorbarh(aes(xmin = elpd_diff - 2 * se_diff, xmax = elpd_diff + 2 * se_diff,
                       y = Model)) +
    geom_point(aes(x = elpd_diff, y = Model)) +
    geom_vline(xintercept = 0, col = "blue") +
    scale_x_continuous("Pointwise Difference in Expected Log Posterior Density (Summed)")
  return(plt)
}

#### LPD Differences ####
elpd_compare(do.call(cbind, lapply(waics_l, function(x) x$loo$pointwise[, 1])),
             refname = "interceptsonly") %>%
  as_tibble(rownames = "Model") %>%
  dplyr::mutate(modeltype = modelname2type(Model)) %>%
  dplyr::inner_join(targetmods, by = c(Model = "shortname"), copy = TRUE) %>%
  dplyr::mutate(longname = factor(longname, levels = targetmods[, "longname"], ordered = TRUE)) %>%
  ggplot() +
  facet_grid(rows = vars(modeltype), scales = "free", space = "free", switch = "y") +
  theme(strip.text.y.left = element_text(angle = 0)) +
  geom_errorbarh(aes(xmin = elpd_diff - 2 * se_diff, xmax = elpd_diff + 2 * se_diff,
                     y = longname)) +
  geom_point(aes(x = elpd_diff, y = longname)) +
  geom_vline(xintercept = 0, col = "blue") +
  scale_y_discrete(name = "") +
  scale_x_continuous("Sum of Difference in E[lpd]")

#### Holdout Richness ####
obsnumbers <- detectednumspec(inputdata$holdoutdata$yXobs[, inputdata$species], 
                              inputdata$holdoutdata$yXobs[, "ModelSiteID", drop = TRUE])
Enum_det <- do.call(cbind, lapply(Enums_holdout, function(x) x["Esum_det", , drop = TRUE]))
Vnum_det <- do.call(cbind, lapply(Enums_holdout, function(x) x["Vsum_det", , drop = TRUE]))

Enum_compare(obsnumbers, Enum_det, Vnum_det)
meanplt <- Enum_compare(obsnumbers, Enum_det, Vnum_det) %>%
  as_tibble(rownames = "Model") %>%
  dplyr::mutate(modeltype = modelname2type(Model)) %>%
  tidyr::pivot_longer(starts_with("SE"), names_to = "SEtype", values_to = "SE") %>%
  ggplot() +
  facet_grid(rows = vars(modeltype), scales = "free_y", space = "free_y") +
  geom_vline(xintercept = 0, col = "blue") +
  geom_errorbarh(aes(xmin = `E[D]_obs` - 2 * SE, xmax = `E[D]_obs` + 2 * SE,
                   y = Model,
                   col = SEtype, lty = SEtype)) +
  geom_point(aes(x = `E[D]_obs`, y = Model)) +
  scale_x_continuous("Mean Residual") +
  ggtitle("Holdout: Mean Residual of Number of Detected Spccies")

varplt <- Enum_compare(obsnumbers, Enum_det, Vnum_det) %>%
  as_tibble(rownames = "Model") %>%
  dplyr::mutate(modeltype = modelname2type(Model)) %>%
  tidyr::pivot_longer(starts_with("V"), names_to = "Vtype", values_to = "Variance") %>%
  ggplot() +
  facet_grid(rows = vars(modeltype), scales = "free_y", space = "free_y") +
  geom_point(aes(x = sqrt(Variance), y = Model, col = Vtype)) +
  scale_x_continuous("Standard Deviation of Residual") +
  ggtitle("Holdout: Standard Deviation of Number of Detected Species")

print(meanplt / varplt)

###### In Sample Richness #####
obsnumbers <- detectednumspec(inputdata$insampledata$yXobs[, inputdata$species], 
                              inputdata$insampledata$yXobs[, "ModelSiteID", drop = TRUE])
Enum_det <- do.call(cbind, lapply(Enums_insample, function(x) x["Esum_det", , drop = TRUE]))
Vnum_det <- do.call(cbind, lapply(Enums_insample, function(x) x["Vsum_det", , drop = TRUE]))

Enum_compare(obsnumbers, Enum_det, Vnum_det)
meanplt <- Enum_compare(obsnumbers, Enum_det, Vnum_det) %>%
  as_tibble(rownames = "Model") %>%
  dplyr::mutate(modeltype = modelname2type(Model)) %>%
  tidyr::pivot_longer(starts_with("SE"), names_to = "SEtype", values_to = "SE") %>%
  ggplot() +
  facet_grid(rows = vars(modeltype), scales = "free_y", space = "free_y") +
  geom_vline(xintercept = 0, col = "blue") +
  geom_errorbarh(aes(xmin = `E[D]_obs` - 2 * SE, xmax = `E[D]_obs` + 2 * SE,
                   y = Model,
                   col = SEtype, lty = SEtype)) +
  geom_point(aes(x = `E[D]_obs`, y = Model)) +
  scale_x_continuous("Mean Residual") +
  ggtitle("Insample: Mean Residual of Number of Detected Spccies")

varplt <- Enum_compare(obsnumbers, Enum_det, Vnum_det) %>%
  as_tibble(rownames = "Model") %>%
  dplyr::mutate(modeltype = modelname2type(Model)) %>%
  tidyr::pivot_longer(starts_with("V"), names_to = "Vtype", values_to = "Variance") %>%
  ggplot() +
  facet_grid(rows = vars(modeltype), scales = "free_y", space = "free_y") +
  geom_point(aes(x = sqrt(Variance), y = Model, col = Vtype)) +
  scale_x_continuous("Standard Deviation of Residual") +
  ggtitle("Insample: Standard Deviation of Number of Detected Species")

print(meanplt / varplt)

#### Occupancy Residuals of Best ####
bestwdet <- readRDS("./tmpdata/7_3_02_clim_someclimate_year_woody500m_msnm_det1stO.rds")
seeds = c(321, 120)
df_l <- lapply(seeds, function(x) {
  resids <- ds_occupancy_residuals.fit(bestwdet, type = "median", seed = x, conditionalLV = (!is.null(bestwdet$nlv) && bestwdet$nlv > 0))
  df <- resids %>%
    tidyr::pivot_longer(-c(ModelSite),
                        names_to = "Species",
                        values_to = "Residual",
                        values_drop_na = TRUE) %>%
    left_join(occcovar %>% 
                tidyr::pivot_longer(-ModelSite,
                                    names_to = "Covariate",
                                    values_to = "CovariateValue"),
              by = "ModelSite")
  return(df)
})
names(df_l) <- seeds
df <- bind_rows(df_l, .id = "seed")
rm(df_l)

treatfactor <- df %>%
  group_by(Covariate) %>%
  summarise(nvals = n_distinct(CovariateValue), .groups = "drop_last") %>%
  filter(nvals <= 10) %>%
  dplyr::select(Covariate) %>%
  unlist()

plt <- df %>%
  ggplot() +
  ggplot2::geom_point(aes(x = CovariateValue, y = Residual), data = function(x) x[x$seed == seeds[[1]], ])

# gam smooths for continuous variables
plt <- plt + ggplot2::geom_smooth(aes(x = CovariateValue, y = Residual, col = seed),
                                  data = function(x) x %>% dplyr::filter(!(Covariate %in% treatfactor)),
                                  method = "gam", level = 0.95, formula = y ~ s(x, bs = "cs"))

# mean + 2SE summaries for discrete variables
plt <- plt + ggplot2::stat_summary(aes(x = CovariateValue, y = Residual, col = seed),
                                   data = function(x) x %>% dplyr::filter(Covariate %in% treatfactor),
                                   geom = "pointrange",
                                   fun.data = mean_se,
                                   fun.args = list(mult = 2),
                                   position = position_dodge(width = 0.1, preserve = "total"),
                                   alpha = 0.8,
                                   lwd = 1.2,
                                   fatten = 1,
                                   show.legend = FALSE)

plt <- plt +
  ggplot2::facet_wrap(vars(Covariate), scales = "free_x") +
  ggplot2::geom_hline(yintercept = 0, col = "blue", lty = "dashed") +
  ggplot2::scale_x_continuous(name = "Covariate Value") +
  scale_y_continuous(name = "Occupancy Residual")
plt <- plt + coord_cartesian(ylim = c(-0.1, 0.1)) + ggtitle("Occupancy Residuals vs Covariates") +
  theme(strip.text.y.right = element_text(angle = 0))

print(plt)