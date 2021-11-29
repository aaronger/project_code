source(here::here("code", "pkgs_and_common.R"))
source(here::here("code", "quantgen.R"))

lags = qr_lags
forecast_dates <- qr_forecast_dates
tau = c(0.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

# To user with future_map, we must evaluate these globals ahead of time
# This may not be necessary or helpful for working with local data
make_start_day_ar = function(ahead, ntrain, lags) {
  offset = eval(1 - max(ahead) - 
    ntrain - max(lags))
  start_day_ar = function(forecast_date) {
    return(as.Date(forecast_date) + offset)
  }
  return(start_day_ar)
}

start_day_ar = make_start_day_ar(ahead, ntrain, lags)
preds_dir <- here::here("data", "predictions", "quantreg")
if (!dir.exists(preds_dir)) dir.create(preds_dir)

train_type <- "honest"
offline_signal_dir = here::here("data", "offline_signals", sprintf('%s_as_of', train_type))
st_dir <- here(offline_signal_dir, "county_weekly") 
debug_dir = here::here("data", "debug_results", "quantreg", sprintf("%s_as_of", train_type))
if (!dir.exists(debug_dir)) {dir.create(debug_dir, recursive = TRUE)}

model_rows <- list(
  1,
  c(1,2)
)

for (rows in model_rows) {
  signals_ar = signals_df(rows)
  name <- model_name(signals_ar)
  message(name)
  t0 = Sys.time()
    preds <- offline_get_predictions(
      forecast_dates = forecast_dates,
      forecaster = quantgen_forecaster,
      signals = signals_ar,
      offline_signal_dir = st_dir,
      forecaster_args = list(
        # forecast_date and df_list(forecast_date) added to this list 
        # by internal function offline_get_predictions_single_date
        signals = signals_ar,
        incidence_period = incidence_period,
        ahead = ahead,
        geo_type = geo_type,
        tau = tau,
        n = ntrain,
        lags = lags,
        lambda = 0,
        lp_solver = lp_solver,
        sort = TRUE,
        #zero_impute = some list to be computed from signals_ar,
        #resample = str_detect(tt, "bootstrapped"),
        debug = here::here(debug_dir, name)
      ),
      name_of_forecaster = name,
      incidence_period = incidence_period,
    )
  t1 = Sys.time()
  print(t1-t0)

  saveRDS(preds, here(preds_dir, sprintf('%s_%s.RDS', name, train_type)))
}
