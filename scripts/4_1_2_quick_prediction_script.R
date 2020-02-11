############################################################
#################### applying model prediction #############
############################################################
# load the cannon form of the predictors
predictors_cannon_form = list(intercept = 1,
                              gpp_mean = brick(paste0("./tmpdata/4_1_predictors_cannon_form_", "gpp_mean", ".grd")),
                              m1b_resid =  brick(paste0("./tmpdata/4_1_predictors_cannon_form_", "m1b_resid", ".grd")),
                              log_plus_one_woody_cover = brick(paste0("./tmpdata/4_1_predictors_cannon_form_", "log_plus_one_woody_cover", ".grd")),
                              "gpp_mean:log_plus_one_woody_cover" = brick(paste0("./tmpdata/4_1_predictors_cannon_form_", "gpp_mean.log_plus_one_woody_cover", ".grd")),
                              year = brick(paste0("./tmpdata/4_1_predictors_cannon_form_", "year", ".grd")))


# load the coefficients of the fitted model
boral_coefficients_matrix <- readRDS("./private/coefficients/4_1_script_boral_coefficients_matrix.rds")

# function that weights predictors by coefficients and then sums, given a species (rowname)
#' @predictors_cannon_form is a named list of raster bricks containing the predictors
wght_sum_pnorm <- function(rowname, year, coefficient_mat, predictors_cannon_form){
  coefficients <- coefficient_mat[rowname, ]
  lyrname <- paste0("X",year,".09.06")
  predictors_cannon_form[["m1b_resid"]] <- predictors_cannon_form[["m1b_resid"]][[lyrname]]
  predictors_cannon_form[["log_plus_one_woody_cover"]] <- predictors_cannon_form[["log_plus_one_woody_cover"]][[lyrname]]
  predictors_cannon_form[["gpp_mean:log_plus_one_woody_cover"]] <- predictors_cannon_form[["gpp_mean:log_plus_one_woody_cover"]][[lyrname]]
  predictors_cannon_form[["year"]] <- predictors_cannon_form[["year"]][[lyrname]]
  
  names(coefficients)[names(coefficients) == "date"] <- "year"
  wght_pred <- mapply("*", coefficients, predictors_cannon_form[names(coefficients)], SIMPLIFY = FALSE)
  linearpred <- Reduce("+", wght_pred)
  prediction <- calc(linearpred, pnorm)
  names(prediction) <- lyrname
  return(prediction)
}

predlyr <- wght_sum_pnorm(rowname = "Australasian Pipit", 
               year = 2003,
               coefficient_mat = boral_coefficients_matrix,
               predictors_cannon_form = predictors_cannon_form)


#################################################################
##### Check Predictions #########################################
#################################################################
library(boral)
preds <- readRDS("./private/predictions/4_1_boral_model_fitted_response.rds")
## Find out date that corresponds to Sept 6th
testspecies <- "Grey Fantail"
testvisits <- which( (month(preds$SurveyDate) == 9) & (day(preds$SurveyDate) %in% 1:12) )
preds[testvisits, c("SurveyDate", testspecies)]  #all are in year 2011
rasterpred_lyr <- wght_sum_pnorm(testspecies, 
                                        2011,
                                        coefficient_mat = boral_coefficients_matrix,
                                        predictors_cannon_form = predictors_cannon_form)
rasterpred_points <- raster::extract(rasterpred_lyr, preds[testvisits, c("longitude", "latitude")])
cbind(preds[testvisits, c("SurveyDate", testspecies)], rasterpred_points)
stopifnot(all.equal(preds[testvisits, testspecies], rasterpred_points, tolerance = 0.1))
# These predictions *should* be close! But they aren't and there is even an NA value!!
