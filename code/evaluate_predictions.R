library(tibble)
library(dplyr)
library(evalcast)
library(covidcast)

actuals = readRDS(here::here("data", "actuals_casesProp_county_weekly.RDS"))

tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)
p1 <- readRDS(here::here("data", "predictions", "quantreg",
                         "AR3casesProp_honest.RDS"))
p2 <- readRDS(here::here("data", "predictions", "quantreg",
                         "AR3casesPropBord_honest.RDS"))

evals_AR3casesProp = evaluate_predictions(
  p1,
  actuals,
  err_measures = list(ae=absolute_error,
                      wis=weighted_interval_score),
  grp_vars = c("forecaster", "forecast_date", "ahead", "geo_value"))

evals_AR3casesPropBord = evaluate_predictions(
  p2,
  actuals,
  err_measures = list(ae=absolute_error,
                      wis=weighted_interval_score),
  grp_vars = c("forecaster", "forecast_date", "ahead", "geo_value"))

evals_AR3casesPropLEX = evaluate_predictions(
  readRDS(here::here("data", "predictions", "quantreg",
                     "AR3casesPropLEX_honest.RDS")),
  actuals,
  err_measures = list(ae=absolute_error,
                      wis=weighted_interval_score),
  grp_vars = c("forecaster", "forecast_date", "ahead", "geo_value"))

saveRDS(evals_AR3casesProp, here::here("data", "evaluations", "evals_AR3casesProp"))
saveRDS(evals_AR3casesPropBord, here::here("data", "evaluations", "evals_AR3casesPropBord"))
saveRDS(evals_AR3casesPropLEX, here::here("data", "evaluations", "evals_AR3casesPropLEX"))

AR_models = c(
      'AR3',
      'AR3bord')

# Set train type
train_types = c('honest', 'dishonest')

for (tt in train_types) {
  # Read baseline
  train_type <- stringr::str_extract(tt, "[^_]*")
  baseline_preds = readRDS(
    here::here("data", "predictions", "quantreg", sprintf('Baseline_%s.RDS', train_type))
  )

  # Subset baseline quantiles (properly!)
  baseline_preds_subset = baseline_preds %>%
    filter (as.character(quantile*1000) %in% as.character(tau*1000))


  # Ingest AR predictions
  preds_list = vector('list', length(AR_models)+1)

  for (idx in 1:length(AR_models)) {
    preds_list[[idx]] = readRDS(
      here::here("data","predictions", "quantreg",
                 sprintf('%s_%s.RDS', AR_models[[idx]], tt))
    )
  }

  preds_list[[length(preds_list)]] = baseline_preds_subset



  # Only NAs for the subset training, as expected.  Hence
  # we filter out NAs for that one and proceed.

  preds_list[[7]] = preds_list[[7]] %>% filter(!is.na(value))
  preds = bind_rows(preds_list)

  rm(preds_list)

  if (tt == "honest") {
    saveRDS(preds, here::here("data", sprintf('predictions_%s.RDS', tt)))
  }

  results = evaluate_predictions(
    preds,
    actuals,
    err_measures = list(ae=absolute_error,
                        wis=weighted_interval_score),
    grp_vars = c("forecaster", "forecast_date", "ahead", "geo_value"))
  results <- results %>% select(-data_source, -signal, -incidence_period)

  saveRDS(results, here::here("data", sprintf('results_%s.RDS', tt)))
}

