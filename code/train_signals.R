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
    return(as.Date(forecast_date) + offset*ifelse(incidence_period == "epiweek", 7, 1))
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

for (idx in 1:nrow(signals_df)) {
  signals_ar = tibble::tibble( 
    # a 1 or 2 row tibble for response only or response plus signal;
    # used to build names of RDS files to be loaded for each forecast_date
    data_source = unique(c(response_data_source, signals_df$data_source[idx])),
    signal = unique(c(response_signal, signals_df$signal[idx])),
    graph = unique(c("id", signals_df$graph[idx])),
    start_day = list(start_day_ar),
    geo_values = list(signals_df$geo_values[[idx]]),
    geo_type = geo_type
  )
  message(signals_df$name[idx])
  t0 = Sys.time()
    preds <- offline_get_predictions(
      forecast_dates = forecast_dates[57],
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
        zero_impute = signals_df$zero_impute[[idx]],
          #resample = str_detect(tt, "bootstrapped"),
        debug = here::here(debug_dir, signals_df$name[idx])
      ),
      name_of_forecaster = signals_df$name[idx],
      incidence_period = incidence_period,
    )
  t1 = Sys.time()
  print(t1-t0)

  saveRDS(preds, here(preds_dir, sprintf('%s_%s.RDS', signals_df$name[idx], train_type)))
}
