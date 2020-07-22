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
library(ggrepel)

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
  "os_msnm_gc",    "Overstorey, Ground Cover and\nNoisy Miner Interacting with Midstorey",
  "msnm",    "Noisy Miner Interacting with Midstorey",
  "nm",    "Only Noisy Miner",
  "twiwoody500m",    "Terrain Wetness and Nearby Tree Canopy",
  "woody500m",    "Nearby Tree Canopy",
  "gppmean",    "Mean Gross Primary Productivity",
  "someclimate_year_woody500m_msnm", "Mean, Arid, Cold, and Stability Hypotheses,\nNearby Tree Canopy, Noisy Miner Interacting with Midstorey"
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
plt <- elpd_compare(do.call(cbind, lapply(waics_l, function(x) x$loo$pointwise[, 1])),# %>%
             refname = "interceptsonly") %>%
  as_tibble(rownames = "Model") %>%
  dplyr::mutate(modeltype = modelname2type(Model)) %>%
  dplyr::inner_join(targetmods, by = c(Model = "shortname"), copy = TRUE) %>%
  dplyr::mutate(longname = factor(longname, levels = targetmods[, "longname"], ordered = TRUE)) %>%
  dplyr::filter(modeltype != "") %>%
  ggplot() +
  facet_grid(rows = vars(modeltype), scales = "free", space = "free", switch = "y") +
  theme(strip.text.y.left = element_blank()) +
  geom_errorbarh(aes(xmin = elpd_diff - 2 * se_diff, xmax = elpd_diff + 2 * se_diff,
                     y = longname)) +
  geom_point(aes(x = elpd_diff, y = longname)) +
  geom_vline(xintercept = 0, col = "blue") +
  scale_y_discrete(name = "") +
  scale_x_continuous("Sum of Difference in E[lpd]")
ggsave("./private/plots/model_selection_results.pdf",
       plot = plt,
       width = 7, height = 5, units = "in")
