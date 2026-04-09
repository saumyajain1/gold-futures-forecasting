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

seasonality_decision <- tibble(
  decision = "Do not fit Holt-Winters seasonal ETS",
  reason = "The series is on an irregular trading-day calendar and earlier EDA did not show strong stable seasonality."
)

print_section("ETS scope decision", seasonality_decision)

ets_functions <- list(
  ses = function(y, h) ses(ts(y), h = h, level = c(80, 95)),
  holt = function(y, h) holt(ts(y), h = h, level = c(80, 95)),
  damped_holt = function(y, h) holt(ts(y), h = h, damped = TRUE, level = c(80, 95)),
  ets_auto = function(y, h) {
    fit <- ets(ts(y), model = "ZZN", damped = NULL, restrict = TRUE)
    forecast(fit, h = h, level = c(80, 95))
  }
)

train_y <- train_data$gold_close
holdout_y <- holdout_data$gold_close

training_fits <- lapply(ets_functions, function(fit_fun) {
  fit_fun(train_y, nrow(holdout_data))
})

training_metrics <- bind_rows(lapply(names(training_fits), function(model_name) {
  fit <- training_fits[[model_name]]

  tibble(
    sample = "train",
    model = model_name,
    method = fit$model$method,
    rmse = rmse(train_y, fitted(fit)),
    n_predictions = sum(!is.na(fitted(fit))),
    coverage80 = NA_real_,
    coverage95 = NA_real_,
    avg_width80 = NA_real_,
    avg_width95 = NA_real_,
    aicc = fit$model$aicc
  )
}))

ets_residual_diagnostics <- bind_rows(lapply(names(training_fits), function(model_name) {
  fit <- training_fits[[model_name]]
  residuals <- as.numeric(stats::na.omit(residuals(fit)))
  lb_lag <- min(10, floor(length(residuals) / 5))

  tibble(
    model = model_name,
    method = fit$model$method,
    aicc = fit$model$aicc,
    residual_mean = mean(residuals),
    residual_sd = sd(residuals),
    residual_acf1 = acf(residuals, plot = FALSE)$acf[2],
    ljung_box_pvalue = Box.test(residuals, lag = lb_lag, type = "Ljung-Box")$p.value
  )
}))

rolling_holdout_forecasts <- function(model_name) {
  point_forecast <- rep(NA_real_, nrow(holdout_data))
  lo80 <- rep(NA_real_, nrow(holdout_data))
  hi80 <- rep(NA_real_, nrow(holdout_data))
  lo95 <- rep(NA_real_, nrow(holdout_data))
  hi95 <- rep(NA_real_, nrow(holdout_data))

  for (i in seq_len(nrow(holdout_data))) {
    history_y <- c(train_y, head(holdout_y, i - 1))
    forecast_object <- ets_functions[[model_name]](history_y, 1)

    point_forecast[i] <- as.numeric(forecast_object$mean[1])
    lo80[i] <- as.numeric(forecast_object$lower[1, "80%"])
    hi80[i] <- as.numeric(forecast_object$upper[1, "80%"])
    lo95[i] <- as.numeric(forecast_object$lower[1, "95%"])
    hi95[i] <- as.numeric(forecast_object$upper[1, "95%"])
  }

  tibble(
    date = holdout_data$date,
    model = model_name,
    actual = holdout_y,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )
}

ets_holdout_forecasts <- bind_rows(lapply(names(ets_functions), rolling_holdout_forecasts))

holdout_metrics <- ets_holdout_forecasts |>
  mutate(
    covered80 = actual >= lo80 & actual <= hi80,
    covered95 = actual >= lo95 & actual <= hi95
  ) |>
  group_by(model) |>
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
    training_metrics |> select(model, method, aicc),
    by = "model"
  ) |>
  mutate(sample = "holdout") |>
  select(sample, model, method, rmse, n_predictions, coverage80, coverage95, avg_width80, avg_width95, aicc)

ets_metrics <- bind_rows(training_metrics, holdout_metrics)

ets_vs_baseline <- bind_rows(
  baseline_metrics |>
    filter(sample == "holdout", model == "naive") |>
    mutate(method = "Naive baseline", model_class = "baseline"),
  holdout_metrics |>
    mutate(model_class = "ets")
) |>
  select(model_class, sample, model, method, rmse, coverage80, coverage95, avg_width80, avg_width95, aicc) |>
  arrange(rmse)

ets_metrics_display <- ets_metrics |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
      ~ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))
    )
  )

ets_residual_diagnostics_display <- ets_residual_diagnostics |>
  mutate(across(c(aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue), ~sprintf("%.3f", .x)))

ets_vs_baseline_display <- ets_vs_baseline |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
      ~ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))
    )
  )

write_csv(ets_metrics, file.path(processed_dir, "ets_metrics.csv"))
write_csv(
  ets_residual_diagnostics,
  file.path(processed_dir, "ets_residual_diagnostics.csv")
)
write_csv(
  ets_holdout_forecasts,
  file.path(processed_dir, "ets_holdout_forecasts.csv")
)

print_section("ETS metrics", ets_metrics_display, n = nrow(ets_metrics_display))
print_section(
  "ETS residual diagnostics",
  ets_residual_diagnostics_display,
  n = nrow(ets_residual_diagnostics_display)
)
print_section(
  "ETS vs naive baseline",
  ets_vs_baseline_display,
  n = nrow(ets_vs_baseline_display)
)

ets_plot_data <- bind_rows(
  tail(train_data, 120) |>
    transmute(date, series = "actual", value = gold_close),
  holdout_data |>
    transmute(date, series = "actual", value = gold_close),
  ets_holdout_forecasts |>
    transmute(date, series = model, value = point_forecast)
)

ets_plot <- ggplot(ets_plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = max(train_data$date), linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c(
      actual = "black",
      ses = "steelblue",
      holt = "darkgreen",
      damped_holt = "firebrick",
      ets_auto = "darkorange"
    )
  ) +
  labs(
    title = "ETS Forecasts on the Holdout Period",
    x = "Date",
    y = "Gold close",
    color = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  ets_plot,
  file.path(figures_dir, "ets_holdout_forecasts.png"),
  width = 10,
  height = 6
)

best_ets_model <- holdout_metrics |>
  arrange(rmse) |>
  slice(1) |>
  pull(model)

best_ets_holdout <- ets_holdout_forecasts |>
  filter(model == best_ets_model)

best_ets_plot <- ggplot(best_ets_holdout, aes(x = date, y = point_forecast)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "grey85") +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "grey70") +
  geom_line(aes(y = actual), color = "black", linewidth = 0.4) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(
    title = paste("Best ETS Model:", best_ets_model),
    subtitle = paste("Training fit:", training_fits[[best_ets_model]]$model$method),
    x = "Date",
    y = "Gold close"
  ) +
  theme_minimal()

show_and_save_plot(
  best_ets_plot,
  file.path(figures_dir, "best_ets_prediction_intervals.png"),
  width = 10,
  height = 6
)

if (interactive()) {
  for (model_name in names(training_fits)) {
    message("\nResidual diagnostics plots: ", model_name)
    forecast::checkresiduals(training_fits[[model_name]]$model)
  }
}

message(
  "\nBest ETS model on holdout RMSE: ", best_ets_model,
  " (", training_fits[[best_ets_model]]$model$method, ")",
  "\nSaved metrics to ", file.path(processed_dir, "ets_metrics.csv"),
  "\nSaved residual diagnostics to ", file.path(processed_dir, "ets_residual_diagnostics.csv"),
  "\nSaved holdout forecasts to ", file.path(processed_dir, "ets_holdout_forecasts.csv"),
  "\nSaved ETS plots to ", figures_dir
)
