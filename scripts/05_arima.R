if (file.exists("scripts/utils.R")) {
  source("scripts/utils.R")
} else {
  source("utils.R")
}

project_root <- setup_project(c("forecast", "readr", "dplyr", "ggplot2"))

processed_dir <- file.path(project_root, "data", "processed")
figures_dir <- file.path(project_root, "figures")

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

train_data <- read_csv(
  file.path(processed_dir, "train_data.csv"),
  show_col_types = FALSE
)

holdout_data <- read_csv(
  file.path(processed_dir, "holdout_data.csv"),
  show_col_types = FALSE
)

baseline_metrics <- read_csv(
  file.path(processed_dir, "baseline_metrics.csv"),
  show_col_types = FALSE
)

ets_metrics <- read_csv(
  file.path(processed_dir, "ets_metrics.csv"),
  show_col_types = FALSE
)

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

auto_fit <- auto.arima(
  train_ts,
  seasonal = FALSE,
  stepwise = FALSE,
  approximation = FALSE
)

auto_order <- arimaorder(auto_fit)
auto_has_drift <- "drift" %in% names(auto_fit$coef)

p_neighbor <- if (auto_order[1] == 0) 1 else auto_order[1] - 1
q_neighbor <- if (auto_order[3] == 0) 1 else auto_order[3] - 1

candidate_orders <- unique(rbind(
  c(auto_order[1], auto_order[2], auto_order[3]),
  c(p_neighbor, auto_order[2], auto_order[3]),
  c(auto_order[1], auto_order[2], q_neighbor),
  c(p_neighbor, auto_order[2], q_neighbor)
))

colnames(candidate_orders) <- c("p", "d", "q")

candidate_specs <- as_tibble(candidate_orders) |>
  mutate(
    model = ifelse(
      p == auto_order[1] & d == auto_order[2] & q == auto_order[3],
      "auto_selected",
      paste0("arima_", p, d, q)
    ),
    include_drift = auto_has_drift & d == 1,
    method = ifelse(
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
    fit_candidate(y, p, d, q, include_drift, method = "ML"),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    fit <- tryCatch(
      fit_candidate(y, p, d, q, include_drift, method = "CSS"),
      error = function(e) NULL
    )
  }

  fit
}

training_fits <- lapply(seq_len(nrow(candidate_specs)), function(i) {
  spec <- candidate_specs[i, ]

  fit <- tryCatch(
    safe_fit_candidate(train_y, spec$p, spec$d, spec$q, spec$include_drift),
    error = function(e) NULL
  )

  if (is.null(fit)) {
    return(NULL)
  }

  list(model = spec$model, method = spec$method, p = spec$p, d = spec$d, q = spec$q, fit = fit)
})

training_fits <- training_fits[!vapply(training_fits, is.null, logical(1))]

candidate_specs <- candidate_specs |>
  filter(model %in% vapply(training_fits, function(x) x$model, character(1)))

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
  residuals <- as.numeric(stats::na.omit(residuals(item$fit)))
  lb_lag <- min(10, floor(length(residuals) / 5))

  tibble(
    model = item$model,
    method = item$method,
    aicc = item$fit$aicc,
    residual_mean = mean(residuals),
    residual_sd = sd(residuals),
    residual_acf1 = acf(residuals, plot = FALSE)$acf[2],
    ljung_box_pvalue = Box.test(
      residuals,
      lag = lb_lag,
      type = "Ljung-Box",
      fitdf = item$p + item$q
    )$p.value
  )
}))

rolling_holdout_forecasts <- function(spec) {
  point_forecast <- rep(NA_real_, nrow(holdout_data))
  lo80 <- rep(NA_real_, nrow(holdout_data))
  hi80 <- rep(NA_real_, nrow(holdout_data))
  lo95 <- rep(NA_real_, nrow(holdout_data))
  hi95 <- rep(NA_real_, nrow(holdout_data))

  for (i in seq_len(nrow(holdout_data))) {
    history_y <- c(train_y, head(holdout_y, i - 1))
    fit <- safe_fit_candidate(history_y, spec$p, spec$d, spec$q, spec$include_drift)

    if (is.null(fit)) {
      next
    }

    fc <- forecast(fit, h = 1, level = c(80, 95))

    point_forecast[i] <- as.numeric(fc$mean[1])
    lo80[i] <- as.numeric(fc$lower[1, "80%"])
    hi80[i] <- as.numeric(fc$upper[1, "80%"])
    lo95[i] <- as.numeric(fc$lower[1, "95%"])
    hi95[i] <- as.numeric(fc$upper[1, "95%"])
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

holdout_metrics <- arima_holdout_forecasts |>
  mutate(
    covered80 = actual >= lo80 & actual <= hi80,
    covered95 = actual >= lo95 & actual <= hi95
  ) |>
  group_by(model, method) |>
  summarise(
    rmse = rmse(actual, point_forecast),
    n_predictions = sum(!is.na(point_forecast)),
    coverage80 = mean(covered80, na.rm = TRUE),
    coverage95 = mean(covered95, na.rm = TRUE),
    avg_width80 = mean(hi80 - lo80, na.rm = TRUE),
    avg_width95 = mean(hi95 - lo95, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    training_metrics |> select(model, aicc),
    by = "model"
  ) |>
  mutate(sample = "holdout") |>
  select(sample, model, method, rmse, n_predictions, coverage80, coverage95, avg_width80, avg_width95, aicc)

arima_metrics <- bind_rows(training_metrics, holdout_metrics)

existing_benchmarks <- bind_rows(
  baseline_metrics |>
    filter(sample == "holdout", model == "naive") |>
    mutate(method = "Naive baseline", aicc = NA_real_, model_class = "baseline"),
  ets_metrics |>
    filter(sample == "holdout") |>
    mutate(model_class = "ets")
)

arima_vs_existing <- bind_rows(
  existing_benchmarks |>
    select(model_class, sample, model, method, rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
  holdout_metrics |>
    mutate(model_class = "arima") |>
    select(model_class, sample, model, method, rmse, coverage80, coverage95, avg_width80, avg_width95, aicc)
) |>
  arrange(rmse)

arima_metrics_display <- arima_metrics |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
      ~ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))
    )
  )

arima_residual_diagnostics_display <- arima_residual_diagnostics |>
  mutate(across(c(aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue), ~sprintf("%.3f", .x)))

arima_vs_existing_display <- arima_vs_existing |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
      ~ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))
    )
  )

write_csv(stationarity_summary, file.path(processed_dir, "arima_stationarity_summary.csv"))
write_csv(candidate_specs, file.path(processed_dir, "arima_candidate_models.csv"))
write_csv(arima_metrics, file.path(processed_dir, "arima_metrics.csv"))
write_csv(
  arima_residual_diagnostics,
  file.path(processed_dir, "arima_residual_diagnostics.csv")
)
write_csv(
  arima_holdout_forecasts,
  file.path(processed_dir, "arima_holdout_forecasts.csv")
)
write_csv(
  arima_vs_existing,
  file.path(processed_dir, "arima_vs_existing.csv")
)

print_section("ARIMA metrics", arima_metrics_display, n = nrow(arima_metrics_display))
print_section(
  "ARIMA residual diagnostics",
  arima_residual_diagnostics_display,
  n = nrow(arima_residual_diagnostics_display)
)
print_section(
  "ARIMA vs existing benchmarks",
  arima_vs_existing_display,
  n = nrow(arima_vs_existing_display)
)

diff_y <- diff(train_y, differences = stationarity_summary$ndiffs[stationarity_summary$test == "kpss"])

png(
  filename = file.path(figures_dir, "arima_differenced_acf_pacf.png"),
  width = 1200,
  height = 1200,
  res = 150
)
par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
plot(diff_y, type = "l", main = "Differenced Gold Close (Training Set)", ylab = NULL, xlab = "Index")
acf(diff_y, main = "ACF of Differenced Gold Close")
pacf(diff_y, main = "PACF of Differenced Gold Close")
dev.off()

if (interactive()) {
  par(mfrow = c(3, 1), mar = c(4, 4, 3, 1))
  plot(diff_y, type = "l", main = "Differenced Gold Close (Training Set)", ylab = NULL, xlab = "Index")
  acf(diff_y, main = "ACF of Differenced Gold Close")
  pacf(diff_y, main = "PACF of Differenced Gold Close")
}

arima_plot_data <- bind_rows(
  tail(train_data, 120) |>
    transmute(date, series = "actual", value = gold_close),
  holdout_data |>
    transmute(date, series = "actual", value = gold_close),
  arima_holdout_forecasts |>
    transmute(date, series = model, value = point_forecast)
)

arima_plot <- ggplot(arima_plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = max(train_data$date), linetype = "dashed", color = "grey40") +
  labs(
    title = "ARIMA Forecasts on the Holdout Period",
    x = "Date",
    y = "Gold close",
    color = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  arima_plot,
  file.path(figures_dir, "arima_holdout_forecasts.png"),
  width = 10,
  height = 6
)

best_arima_model <- holdout_metrics |>
  arrange(rmse) |>
  slice(1) |>
  pull(model)

best_arima_holdout <- arima_holdout_forecasts |>
  filter(model == best_arima_model)

best_arima_method <- holdout_metrics |>
  filter(model == best_arima_model) |>
  pull(method)

best_arima_plot <- ggplot(best_arima_holdout, aes(x = date, y = point_forecast)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "grey85") +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "grey70") +
  geom_line(aes(y = actual), color = "black", linewidth = 0.4) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(
    title = paste("Best ARIMA Model:", best_arima_model),
    subtitle = best_arima_method,
    x = "Date",
    y = "Gold close"
  ) +
  theme_minimal()

show_and_save_plot(
  best_arima_plot,
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

message(
  "\nBest ARIMA model on holdout RMSE: ", best_arima_model,
  " (", best_arima_method, ")",
  "\nSaved stationarity summary to ", file.path(processed_dir, "arima_stationarity_summary.csv"),
  "\nSaved candidate models to ", file.path(processed_dir, "arima_candidate_models.csv"),
  "\nSaved metrics to ", file.path(processed_dir, "arima_metrics.csv"),
  "\nSaved residual diagnostics to ", file.path(processed_dir, "arima_residual_diagnostics.csv"),
  "\nSaved holdout forecasts to ", file.path(processed_dir, "arima_holdout_forecasts.csv"),
  "\nSaved ARIMA comparison table to ", file.path(processed_dir, "arima_vs_existing.csv"),
  "\nSaved plots to ", figures_dir
)
