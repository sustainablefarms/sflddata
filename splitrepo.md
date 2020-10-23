# Mapping How to Split This Repo By Purpose
1) data access, extraction and preparation
  + SQL reading
2) generic model fitting tools (including standardising input data, simulating, likelihood and residuals, extracting predictions and more)
   + also ViF analysis functions, avoid multicollinearity stuff
   + model assessment
3) other?
  + plotting tools
  + reference documents



# Work To Do
Name new package: msod
## Shift files
### In data access, extraction and preparation
+ all of analysis/
+ all of side_projects/
+ all of vignettes/
+ inst/climate_names_table.rds
+ references/
+ scripts/

### In model fitting package
+ inst/modeldescriptions/*
+ inst/rmarkdown/*
+ R
  + as_list_format
  + avoid_multicollinearity
  + bugsdataconversion
  + calcpredictions
  + calcspeciesprobs_singlesitetheta
  + DS_residuals *
  + elpd_compare
  + Enum_compare
  + expected_biodiversity *
  + extend_detectionoccupancy
  + get_theta
  + likelihood
  + LV_covar_plots
  + model_quality_stats
  + numspec_fulldistribution
  + pdetection_occupied
  + poccupy
  + predoccupancy_standalone
  + preddesignmat *
  + run_detectionoccupancy
  + RVofrandomselectRV
  + simulate_fit
  + vif_analysis

+ tests/testthat/
  + benchmark_identicalsitesmodel.Rdata
  + En_condLV.txt
  + En_margLV.txt
 Especrich_condLV.txt
 Especrich_margLV.txt
 lkl_sites.txt
 pdet_theta01.txt
 pocc_theta01_condLV.txt
 pocc_theta01_margLV.txt
 test-bugsvar2array.R
 test-calcs_saved_fit.R
 test-DS_residuals.R
 test-expectedbiodiversity.R
 test-fulldistribution_numspecies.R
 test-likelihoods.R
 test-predcalculations.R
 test-prepdesignmat.R
 test-rundetectionoccupancy.R
 test-simulationmethods.R
 test-standalone.R
 test-whole-identical-sites.R
 test-whole-noiterations.R
 test-whole-variety-nolv.R
 test-whole-variety-wlv-60species.R
 test-whole-variety-wlv.R


## Changes to files
+ version number for msod
+ modeldescription files names
+ variable description names
+ dependencies
  + for msod:
    + suggest: arrangements, Rfast, parallel, runjags, car
  + for data extraction:
    + suggest: all others

