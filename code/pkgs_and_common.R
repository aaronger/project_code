

# Library loading ---------------------------------------------------------


library(furrr)
future::plan(multisession, workers = 4)

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
ahead = 7*(1:4)
response_data_source = 'covidData'
response_signal = 'casesProp' 
incidence_period = 'epiweek'

qr_lags = 7*(0:2)
first_monday <- "2020-07-27"
last_monday <- "2021-10-11"
mondays <- seq(from = as.Date(first_monday), to = as.Date(last_monday), by = 7)

qr_forecast_dates <- mondays

train_types <- c("honest", "dishonest", "honest_bootstrapped")
lp_solver = "gurobi"

# Signals -----------------------------------------------------------------

graphs <- c("Bord","LEX")

signal_lib <- expand_grid(
  data_source = response_data_source, 
  response_signal = response_signal, 
  graph = c("", graphs))

signals_df <- function(rows, lib = signal_lib, geo_values = "*", .geo_type = geo_type) {
  lib %>% slice(rows) %>% 
    mutate(
      signal = paste0(response_signal, graph),
      start_day = list(start_day_ar),
      geo_values = list(geo_values),
      geo_type = .geo_type,
      zero_impute = list(NULL)
    )
}

model_name <- function(sig_df, root = "AR3") {
  end <- sig_df %>% group_by(response_signal) %>% 
    summarise(name = paste0(unique(response_signal), paste(graph, collapse = ""))) %>% 
    summarise(name = paste(name, collapse = "")) %>% pull()
  return(paste0(root, end))
}

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
      data_source = response_data_source,
      signal = response_data_signal,
      target_end_date = get_target_period(
        .data$forecast_date, incidence_period, .data$ahead/7)$end,
      # needs to be before incidence period turned into var
      incidence_period = incidence_period
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

  forecast_date <- lubridate::ymd(forecast_date)
  # add appropriate start_date by evaluating start_day_ar functions;
  # signal_listcols evaluates functions in column named "start_day";
  # this is not being used yet - date restriction being handled by n_train
  # argument to quantgen.
  # So right now all this does is rename forecast_date to as_of
  signals <- evalcast:::signal_listcols(signals, forecast_date)
  # get corresponding list of dfs of signal values
  df_list <- signals %>% pmap(function(...) {
    # note pmap operates on a df by applying .f to each row
    sig <- list(...)
    # so that sig is a list of one row's entries
      signal_fpath = here::here(
        offline_signal_dir,
        sprintf('%s_%s_%s.RDS', 
          sig$data_source, 
          sig$signal, 
          sig$as_of) # see note above
      )
      message(sprintf('Reading signal from disk: %s', signal_fpath))
      return(readRDS(signal_fpath))
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
