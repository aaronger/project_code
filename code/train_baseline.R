source(here::here("code", "pkgs_and_common.R"))
lags = qr_lags
forecast_dates <- qr_forecast_dates



# To user with future_map, we must evaluate these globals ahead of time
make_start_day_baseline = function(ntrain) {
  offset = eval(1 - ntrain*ifelse(incidence_period == "epiweek", 7, 1) - 4)
  start_day_baseline = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_baseline)
}

start_day_baseline = make_start_day_baseline(ntrain)

signals_baseline = tibble::tibble( 
    data_source = response_data_source,
    signal = response_signal,
    graph = "id",
    start_day = list(start_day_baseline),
    geo_values = '*',
    geo_type = geo_type
  )

preds_dir <- here::here("data", "predictions", "quantreg")
if (!dir.exists(preds_dir)) dir.create(preds_dir, recursive = TRUE)


for (train_type in c('honest', 'dishonest')) {
  offline_signal_dir = here::here(
    "data", "offline_signals", sprintf('%s_as_of', train_type))
  st_dir <- here(offline_signal_dir, "county_weekly") 
  t0 = Sys.time()
  preds <- offline_get_predictions(
   forecast_dates[57],
   baseline_forecaster,
   signals_baseline,
   forecaster_args = list(
     incidence_period = incidence_period,
     ahead = ahead),
   offline_signal_dir = st_dir,
   name_of_forecaster = 'Baseline',
   incidence_period = incidence_period
 )
  t1 = Sys.time()
  print(t1-t0)
  saveRDS(preds,
          here::here(preds_dir, sprintf('%s_%s.RDS', 'Baseline', train_type)))
}
  