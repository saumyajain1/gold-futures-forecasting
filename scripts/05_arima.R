source(if (file.exists("scripts/utils.R")) "scripts/utils.R" else "utils.R")

paths <- setup_analysis(c("forecast", "readr", "dplyr", "ggplot2"))
processed_dir <- paths$processed_dir
figures_dir <- paths$figures_dir

split_data <- load_split_data(processed_dir)
train_data <- split_data$train
holdout_data <- split_data$holdout
train_y <- train_data$gold_close
holdout_y <- holdout_data$gold_close
train_ts <- ts(train_y)

stationarity_summary <- tibble(
  test = c("kpss", "adf", "pp"),
  ndiffs = c(
    ndiffs(train_ts, test = "kpss"),
    ndiffs(train_ts, test = "adf"),
    ndiffs(train_ts, test = "pp")
  )
)

auto_fit <- auto.arima(train_ts, seasonal = FALSE, stepwise = FALSE, approximation = FALSE)
auto_order <- arimaorder(auto_fit)
auto_has_drift <- "drift" %in% names(auto_fit$coef)

candidate_orders <- unique(rbind(
  auto_order,
  c(max(1, auto_order[1] - 1), auto_order[2], auto_order[3]),
  c(auto_order[1], auto_order[2], max(1, auto_order[3] - 1)),
  c(max(1, auto_order[1] - 1), auto_order[2], max(1, auto_order[3] - 1))
))

candidate_specs <- as_tibble(candidate_orders, .name_repair = ~c("p", "d", "q")) |>
  mutate(
    model = if_else(
      p == auto_order[1] & d == auto_order[2] & q == auto_order[3],
      "auto_selected",
      paste0("arima_", p, d, q)
    ),
    include_drift = auto_has_drift & d == 1,
    method = if_else(
      include_drift,
      paste0("ARIMA(", p, ",", d, ",", q, ") with drift"),
      paste0("ARIMA(", p, ",", d, ",", q, ")")
    )
  ) |>
  distinct(model, .keep_all = TRUE)

print_section("ARIMA stationarity summary", stationarity_summary, n = nrow(stationarity_summary))
print_section("ARIMA candidate models", candidate_specs, n = nrow(candidate_specs))

fit_candidate <- function(y, p, d, q, include_drift, method = "ML") {
  Arima(
    ts(y),
    order = c(p, d, q),
    include.drift = include_drift,
    include.mean = d == 0,
    method = method
  )
}

safe_fit_candidate <- function(y, p, d, q, include_drift) {
  fit <- tryCatch(
    fit_candidate(y, p, d, q, include_drift, "ML"),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    fit <- tryCatch(
      fit_candidate(y, p, d, q, include_drift, "CSS"),
      error = function(e) NULL
    )
  }

  fit
}

training_fits <- lapply(seq_len(nrow(candidate_specs)), function(i) {
  spec <- candidate_specs[i, ]
  fit <- safe_fit_candidate(train_y, spec$p, spec$d, spec$q, spec$include_drift)

  if (is.null(fit)) {
    return(NULL)
  }

  list(model = spec$model, method = spec$method, p = spec$p, q = spec$q, fit = fit)
})

training_fits <- training_fits[!vapply(training_fits, is.null, logical(1))]
candidate_specs <- candidate_specs |> filter(model %in% vapply(training_fits, `[[`, "", "model"))

training_metrics <- bind_rows(lapply(training_fits, function(item) {
  tibble(
    sample = "train",
    model = item$model,
    method = item$method,
    rmse = rmse(train_y, fitted(item$fit)),
    n_predictions = sum(!is.na(fitted(item$fit))),
    coverage80 = NA_real_,
    coverage95 = NA_real_,
    avg_width80 = NA_real_,
    avg_width95 = NA_real_,
    aicc = item$fit$aicc
  )
}))

arima_residual_diagnostics <- bind_rows(lapply(training_fits, function(item) {
  summarise_residuals(as.numeric(stats::na.omit(residuals(item$fit))), fitdf = item$p + item$q) |>
    mutate(model = item$model, method = item$method, aicc = item$fit$aicc, .before = residual_mean)
}))

rolling_holdout_forecasts <- function(spec) {
  point_forecast <- lo80 <- hi80 <- lo95 <- hi95 <- rep(NA_real_, nrow(holdout_data))

  for (i in seq_len(nrow(holdout_data))) {
    history_y <- c(train_y, head(holdout_y, i - 1))
    fit <- safe_fit_candidate(history_y, spec$p, spec$d, spec$q, spec$include_drift)

    if (is.null(fit)) {
      next
    }

    forecast_object <- forecast(fit, h = 1, level = c(80, 95))
    point_forecast[i] <- as.numeric(forecast_object$mean[1])
    lo80[i] <- as.numeric(forecast_object$lower[1, "80%"])
    hi80[i] <- as.numeric(forecast_object$upper[1, "80%"])
    lo95[i] <- as.numeric(forecast_object$lower[1, "95%"])
    hi95[i] <- as.numeric(forecast_object$upper[1, "95%"])
  }

  tibble(
    date = holdout_data$date,
    model = spec$model,
    method = spec$method,
    actual = holdout_y,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )
}

arima_holdout_forecasts <- bind_rows(lapply(seq_len(nrow(candidate_specs)), function(i) {
  rolling_holdout_forecasts(candidate_specs[i, ])
}))

holdout_metrics <- summarise_forecasts(arima_holdout_forecasts, c("model", "method")) |>
  left_join(training_metrics |> select(model, aicc), by = "model") |>
  mutate(sample = "holdout", .before = model) |>
  select(sample, model, method, rmse, n_predictions, coverage80, coverage95, avg_width80, avg_width95, aicc)

arima_metrics <- bind_rows(training_metrics, holdout_metrics)

write_csv(arima_metrics, file.path(processed_dir, "arima_metrics.csv"))
write_csv(arima_residual_diagnostics, file.path(processed_dir, "arima_residual_diagnostics.csv"))
write_csv(arima_holdout_forecasts, file.path(processed_dir, "arima_holdout_forecasts.csv"))

print_section("ARIMA metrics", format_numeric_table(arima_metrics), n = nrow(arima_metrics))
print_section(
  "ARIMA residual diagnostics",
  format_numeric_table(arima_residual_diagnostics),
  n = nrow(arima_residual_diagnostics)
)

diff_y <- diff(train_y, differences = stationarity_summary$ndiffs[stationarity_summary$test == "kpss"])

save_base_plot(
  file.path(figures_dir, "arima_differenced_acf_pacf.png"),
  code = {
    par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
    plot(diff_y, type = "l", main = "Differenced Gold Close (Training Set)", ylab = NULL, xlab = "Index")
    acf(diff_y, main = "ACF of Differenced Gold Close")
    pacf(diff_y, main = "PACF of Differenced Gold Close")
  }
)

show_base_plot(
  {
    par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
    plot(diff_y, type = "l", main = "Differenced Gold Close (Training Set)", ylab = NULL, xlab = "Index")
    acf(diff_y, main = "ACF of Differenced Gold Close")
    pacf(diff_y, main = "PACF of Differenced Gold Close")
  }
)

show_and_save_plot(
  make_holdout_plot(train_data, holdout_data, arima_holdout_forecasts, "ARIMA Forecasts on the Holdout Period"),
  file.path(figures_dir, "arima_holdout_forecasts.png"),
  width = 10,
  height = 6
)

best_arima_model <- holdout_metrics |> arrange(rmse) |> slice(1) |> pull(model)
best_arima_method <- holdout_metrics |> filter(model == best_arima_model) |> pull(method)

show_and_save_plot(
  make_interval_plot(
    arima_holdout_forecasts |> filter(model == best_arima_model),
    paste("Best ARIMA Model:", best_arima_model),
    best_arima_method
  ),
  file.path(figures_dir, "best_arima_prediction_intervals.png"),
  width = 10,
  height = 6
)

if (interactive()) {
  for (item in training_fits) {
    message("\nResidual diagnostics plots: ", item$model)
    forecast::checkresiduals(item$fit)
  }
}
