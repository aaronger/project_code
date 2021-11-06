

# Library loading ---------------------------------------------------------


library(furrr)
future::plan(multisession)

library(progressr)
library(purrr)
library(dplyr)
library(tibble)
library(covidcast)
library(evalcast)
library(glmnet)


# User parameters ---------------------------------------------------------



geo_type <- "hrr"
ntrain = 21
ahead = 7:21
response_data_source = 'jhu-csse'
response_signal = 'confirmed_7dav_incidence_prop'
incidence_period = "day"

qr_lags = c(0, 7, 14)
qr_forecast_dates <- seq(as.Date('2020-06-09'), as.Date('2021-03-31'), by = "day")
hotspot_forecast_dates <- seq(as.Date("2020-06-16"), as.Date("2021-03-31"), by = "day")

train_types <- c("honest", "dishonest", "honest_bootstrapped")
lp_solver = "gurobi"

# Signals -----------------------------------------------------------------



signals_df_1 = tribble(
  ~data_source,         ~signal,
  response_data_source, response_signal,
  'fb-survey',          'smoothed_hh_cmnty_cli',
  'doctor-visits',      'smoothed_adj_cli',
  'chng',               'smoothed_adj_outpatient_cli',
  'chng',               'smoothed_adj_outpatient_covid',
  'google-symptoms',    'sum_anosmia_ageusia_smoothed_search',
  'google-symptoms',    'sum_anosmia_ageusia_smoothed_search',
)
signals_df_2 = tribble(
  ~geo_values,  ~name,              ~zero_impute,
  '*',          'AR3',              NULL,
  '*',          'AR3FBCLI3',        NULL,
  '*',          'AR3DVCLI3',        NULL,
  '*',          'AR3CHCLI3',        NULL,
  '*',          'AR3CHCOV3',        NULL,
  '*',          'AR3GSSAA3_Zero',   'anosmia',
  '*',          'AR3GSSAA3_Subset', NULL,
)
signals_df = bind_cols(signals_df_1, signals_df_2)


#--------------------------------------------#
# "hack" of evalcast, in order to use already
# downloaded data.
#--------------------------------------------#
offline_get_predictions <- function(
  # data for getting predictions from inner function
  forecast_dates,
  forecaster, # quantgen_lasso
  signals, # response or response plus signal 
  forecaster_args = list(),
  apply_corrections = function(signals) signals,
  offline_signal_dir = offline_signal_dir,
  # data for labelling rows in aggregated output df
  response_data_source = signals$data_source[1],
  response_data_signal = signals$signal[1],
  name_of_forecaster, # will come from name column in signals_df
  incidence_period = c("epiweek", "day")) {

  assert_that(is_tibble(signals), msg = "`signals` should be a tibble.")
  out <- forecast_dates %>%
  map(~ do.call(
    offline_get_predictions_single_date,
    list(
      forecast_date = .x, 
      forecaster = forecaster, 
      signals = signals,
      forecaster_args = forecaster_args,
      apply_corrections = apply_corrections,
      offline_signal_dir = offline_signal_dir
    )
  )) %>%
  bind_rows()
  names(out$value) = NULL
  out <- out %>%
  mutate(
    forecaster = name_of_forecaster,
    incidence_period = incidence_period,
    data_source = response_data_source,
    signal = response_data_signal,
    target_end_date = get_target_period(
      .data$forecast_date, incidence_period, .data$ahead)$end
  ) %>%
  relocate(.data$forecaster, .before = .data$forecast_date)
  class(out) <- c("predictions_cards", class(out))
  out
}

offline_get_predictions_single_date <- function(
  forecast_date,
  forecaster, 
  signals,
  forecaster_args,
  apply_corrections,
  offline_signal_dir) {

  # Addison's spoof of download_signal.
  if (is.null(offline_signal_dir)) {
    download_signal_function = evalcast:::download_signal
  } else {
    download_signal_function = function(
        data_source, 
        signal, 
        start_day, 
        end_day, 
        as_of, 
        geo_type, 
        geo_values) {
      signal_fpath = here::here(
        offline_signal_dir,
        sprintf('%s_%s_%s.RDS', data_source, signal, end_day)
      )
      message(sprintf('Reading signal from disk: %s', signal_fpath))
      return(readRDS(signal_fpath))
    }
  }
  forecast_date <- lubridate::ymd(forecast_date)
  # add appropriate start_date or as_of by evaulating start_day_ar functions
  signals <- evalcast:::signal_listcols(signals, forecast_date)
  # get corresponding df of signal values
  df_list <- signals %>% pmap(function(...) {
    sig <- list(...)
    download_signal_function(
      data_source = sig$data_source, 
      signal = sig$signal,
      start_day = sig$start_day, 
      end_day = forecast_date,
      as_of = sig$as_of, 
      geo_type = sig$geo_type,
      geo_values = sig$geo_values)
  })
  if (!is.null(apply_corrections))
    df_list <- apply_corrections(df_list)
  # pass forecast_date to forecaster for labelling its output file
  forecaster_args$forecast_date = forecast_date
  # pass training df of signal values to forecaster
  forecaster_args$df_list <- df_list
  # get predictions
  out <- do.call(forecaster, forecaster_args)
  assert_that(all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
              msg = paste("Your forecaster must return a data frame with",
                          "(at least) the columnns `ahead`, `geo_value`,",
                          "`quantile`, and `value`."))
  # also identify forecast date in aggregated output df
  out$forecast_date = forecast_date
  return(out)
}
