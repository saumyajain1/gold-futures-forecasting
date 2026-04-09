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

all_data <- bind_rows(
  train_data |> mutate(sample = "train"),
  holdout_data |> mutate(sample = "holdout")
) |>
  mutate(index = row_number())

model_functions <- list(
  mean = function(y) meanf(ts(y), h = 1, level = c(80, 95)),
  naive = function(y) naive(ts(y), h = 1, level = c(80, 95))
)

minimum_history <- c(mean = 1, naive = 1)

rolling_forecasts <- function(df, model_name) {
  y <- df$gold_close
  n <- nrow(df)

  point_forecast <- rep(NA_real_, n)
  lo80 <- rep(NA_real_, n)
  hi80 <- rep(NA_real_, n)
  lo95 <- rep(NA_real_, n)
  hi95 <- rep(NA_real_, n)

  for (i in seq_len(n)) {
    if (i <= minimum_history[[model_name]]) {
      next
    }

    forecast_object <- model_functions[[model_name]](y[1:(i - 1)])

    point_forecast[i] <- as.numeric(forecast_object$mean[1])
    lo80[i] <- as.numeric(forecast_object$lower[1, "80%"])
    hi80[i] <- as.numeric(forecast_object$upper[1, "80%"])
    lo95[i] <- as.numeric(forecast_object$lower[1, "95%"])
    hi95[i] <- as.numeric(forecast_object$upper[1, "95%"])
  }

  df |>
    transmute(
      index,
      date,
      sample,
      model = model_name,
      actual = gold_close,
      point_forecast,
      lo80,
      hi80,
      lo95,
      hi95
    )
}

baseline_forecasts <- bind_rows(lapply(names(model_functions), function(model_name) {
  rolling_forecasts(all_data, model_name)
}))

baseline_metrics <- baseline_forecasts |>
  mutate(
    error = actual - point_forecast,
    covered80 = actual >= lo80 & actual <= hi80,
    covered95 = actual >= lo95 & actual <= hi95
  ) |>
  group_by(sample, model) |>
  summarise(
    rmse = rmse(actual, point_forecast),
    n_predictions = sum(!is.na(point_forecast)),
    coverage80 = mean(covered80, na.rm = TRUE),
    coverage95 = mean(covered95, na.rm = TRUE),
    avg_width80 = mean(hi80 - lo80, na.rm = TRUE),
    avg_width95 = mean(hi95 - lo95, na.rm = TRUE),
    .groups = "drop"
  )

baseline_metrics_display <- baseline_metrics |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95),
      ~sprintf("%.3f", .x)
    )
  )

residual_diagnostics <- baseline_forecasts |>
  filter(sample == "train") |>
  mutate(residual = actual - point_forecast) |>
  group_by(model) |>
  group_modify(~{
    residuals <- .x$residual[!is.na(.x$residual)]
    lb_lag <- min(10, floor(length(residuals) / 5))

    tibble(
      n_residuals = length(residuals),
      residual_mean = mean(residuals),
      residual_sd = sd(residuals),
      residual_acf1 = acf(residuals, plot = FALSE)$acf[2],
      ljung_box_pvalue = Box.test(residuals, lag = lb_lag, type = "Ljung-Box")$p.value
    )
  }) |>
  ungroup()

residual_diagnostics_display <- residual_diagnostics |>
  mutate(across(where(is.numeric), ~sprintf("%.3f", .x)))

holdout_forecasts <- baseline_forecasts |>
  filter(sample == "holdout") |>
  select(-index)

write_csv(baseline_metrics, file.path(processed_dir, "baseline_metrics.csv"))
write_csv(
  residual_diagnostics,
  file.path(processed_dir, "baseline_residual_diagnostics.csv")
)
write_csv(
  holdout_forecasts,
  file.path(processed_dir, "baseline_holdout_forecasts.csv")
)

print_section("Baseline metrics", baseline_metrics_display, n = nrow(baseline_metrics_display))
print_section(
  "Baseline residual diagnostics",
  residual_diagnostics_display,
  n = nrow(residual_diagnostics_display)
)

comparison_plot_data <- bind_rows(
  tail(train_data, 120) |>
    transmute(date, series = "actual", value = gold_close),
  holdout_data |>
    transmute(date, series = "actual", value = gold_close),
  holdout_forecasts |>
    transmute(date, series = model, value = point_forecast)
)

comparison_plot <- ggplot(comparison_plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = max(train_data$date), linetype = "dashed", color = "grey40") +
  scale_color_manual(
    values = c(
      actual = "black",
      mean = "darkorange",
      naive = "steelblue"
    )
  ) +
  labs(
    title = "Baseline Point Forecasts on the Holdout Period",
    x = "Date",
    y = "Gold close",
    color = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  comparison_plot,
  file.path(figures_dir, "baseline_holdout_forecasts.png"),
  width = 10,
  height = 6
)

interval_plot <- ggplot(holdout_forecasts, aes(x = date, y = point_forecast)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "grey85") +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "grey70") +
  geom_line(aes(y = actual), color = "black", linewidth = 0.4) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  facet_wrap(~model, ncol = 1, scales = "free_y") +
  labs(
    title = "Baseline 80% and 95% Prediction Intervals",
    x = "Date",
    y = "Gold close"
  ) +
  theme_minimal()

show_and_save_plot(
  interval_plot,
  file.path(figures_dir, "baseline_prediction_intervals.png"),
  width = 10,
  height = 9
)

if (interactive()) {
  for (model_name in names(model_functions)) {
    train_residuals <- baseline_forecasts |>
      filter(sample == "train", model == model_name) |>
      mutate(residual = actual - point_forecast) |>
      pull(residual)

    message("\nResidual diagnostics plots: ", model_name)
    forecast::checkresiduals(ts(stats::na.omit(train_residuals)))
  }
}

message(
  "\nSaved metrics to ", file.path(processed_dir, "baseline_metrics.csv"),
  "\nSaved residual diagnostics to ", file.path(processed_dir, "baseline_residual_diagnostics.csv"),
  "\nSaved holdout forecasts to ", file.path(processed_dir, "baseline_holdout_forecasts.csv"),
  "\nSaved baseline plots to ", figures_dir
)
