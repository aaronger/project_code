

# Library loading ---------------------------------------------------------


library(furrr)
future::plan(multisession)

library(progressr)
library(purrr)
library(tidyverse)
library(covidcast)
library(evalcast)
library(glmnet)
library(assertthat)
library(here)


# User parameters ---------------------------------------------------------

geo_type <- "county"
ntrain = 12
ahead = 7*(1:3)
response_data_source = 'covidData'
response_signal = 'cases' # next try props
incidence_period = 'epiweek'

qr_lags = c(0, 7, 14)
first_monday <- "2020-07-27"
last_monday <- "2021-10-11"
mondays <- seq(from = as.Date(first_monday), to = as.Date(last_monday), by = 7)

qr_forecast_dates <- mondays

train_types <- c("honest", "dishonest", "honest_bootstrapped")
lp_solver = "gurobi"

# Signals -----------------------------------------------------------------

signals_df_1 = tribble(
  ~data_source,         ~signal,          ~graph,
  response_data_source, response_signal,  'id',
  'covidData',          'cases',          'bord'
)
signals_df_2 = tribble(
  ~geo_values,  ~name,              ~zero_impute,
  '*',          'AR3',              NULL,
  '*',          'AR3bord',          NULL,
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
    target_end_date = forecast_date - 2 + 7*ahead
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
        graph,
        start_day, # stub
        end_day, 
        as_of, # stub
        geo_type, # stub
        geo_values # stub
      ) {
      signal_fpath = here::here(
        offline_signal_dir,
        sprintf('%s_%s_%s_%s.RDS', data_source, signal, graph, end_day)
      )
      message(sprintf('Reading signal from disk: %s', signal_fpath))
      return(readRDS(signal_fpath))
    }
  }
  forecast_date <- lubridate::ymd(forecast_date)
  # add appropriate start_date or as_of by evaulating start_day_ar functions;
  # signal_listcols evaluates functions in column named "start_day" or "as_of"
  signals <- evalcast:::signal_listcols(signals, forecast_date)
  # get corresponding list of dfs of signal values
  df_list <- signals %>% pmap(function(...) {
    # note pmap operates on a df by applying .f to each row
    sig <- list(...)
    # so that sig is a list of one row's entries
    download_signal_function(
      data_source = sig$data_source, 
      signal = sig$signal,
      graph = sig$graph,
      start_day = sig$start_day, # not used for disk read?
      end_day = forecast_date, # seems to be what determines window
      as_of = sig$as_of, 
      geo_type = sig$geo_type,
      geo_values = sig$geo_values)
  })
  if (!is.null(apply_corrections)) {
    df_list <- apply_corrections(df_list)
  }
  # pass forecast_date to forecaster for labelling its output file
  forecaster_args$forecast_date = forecast_date
  # pass training df of signal values to forecaster;
  # will be wrangled by covidcast::agregate_signals
  forecaster_args$df_list <- df_list
  # get predictions
  out <- do.call(forecaster, forecaster_args)
  assert_that(
    all(c("ahead", "geo_value", "quantile", "value") %in% names(out)),
    msg = paste(
      "Your forecaster must return a data frame with",
      "(at least) the columnns `ahead`, `geo_value`,",
      "`quantile`, and `value`."))
  # also identify forecast date in aggregated output df
  out$forecast_date = forecast_date
  return(out)
}
