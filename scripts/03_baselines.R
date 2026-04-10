source(if (file.exists("scripts/utils.R")) "scripts/utils.R" else "utils.R")

paths <- setup_analysis(c("forecast", "readr", "dplyr", "ggplot2"))
processed_dir <- paths$processed_dir
figures_dir <- paths$figures_dir

split_data <- load_split_data(processed_dir)
train_data <- split_data$train
holdout_data <- split_data$holdout

all_data <- bind_rows(
  train_data |> mutate(sample = "train"),
  holdout_data |> mutate(sample = "holdout")
) |>
  mutate(index = row_number())

model_functions <- list(
  mean = function(y) meanf(ts(y), h = 1, level = c(80, 95)),
  naive = function(y) naive(ts(y), h = 1, level = c(80, 95))
)

rolling_forecasts <- function(model_name) {
  y <- all_data$gold_close
  point_forecast <- lo80 <- hi80 <- lo95 <- hi95 <- rep(NA_real_, nrow(all_data))

  for (i in seq(3, nrow(all_data))) {
    forecast_object <- model_functions[[model_name]](y[1:(i - 1)])
    point_forecast[i] <- as.numeric(forecast_object$mean[1])
    lo80[i] <- as.numeric(forecast_object$lower[1, "80%"])
    hi80[i] <- as.numeric(forecast_object$upper[1, "80%"])
    lo95[i] <- as.numeric(forecast_object$lower[1, "95%"])
    hi95[i] <- as.numeric(forecast_object$upper[1, "95%"])
  }

  all_data |>
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

baseline_forecasts <- bind_rows(lapply(names(model_functions), rolling_forecasts))

baseline_metrics <- summarise_forecasts(baseline_forecasts, c("sample", "model"))

residual_diagnostics <- baseline_forecasts |>
  filter(sample == "train") |>
  group_by(model) |>
  group_modify(~ summarise_residuals(.x$actual - .x$point_forecast)) |>
  ungroup() |>
  mutate(n_residuals = nrow(train_data) - 1, .before = residual_mean)

holdout_forecasts <- baseline_forecasts |>
  filter(sample == "holdout") |>
  select(-index)

write_csv(baseline_metrics, file.path(processed_dir, "baseline_metrics.csv"))
write_csv(residual_diagnostics, file.path(processed_dir, "baseline_residual_diagnostics.csv"))
write_csv(holdout_forecasts, file.path(processed_dir, "baseline_holdout_forecasts.csv"))

print_section("Baseline metrics", format_numeric_table(baseline_metrics), n = nrow(baseline_metrics))
print_section(
  "Baseline residual diagnostics",
  format_numeric_table(residual_diagnostics),
  n = nrow(residual_diagnostics)
)

show_and_save_plot(
  make_holdout_plot(
    train_data,
    holdout_data,
    holdout_forecasts,
    "Baseline Point Forecasts on the Holdout Period",
    colors = c(actual = "black", mean = "darkorange", naive = "steelblue")
  ),
  file.path(figures_dir, "baseline_holdout_forecasts.png"),
  width = 10,
  height = 6
)

show_and_save_plot(
  make_interval_plot(holdout_forecasts, "Baseline 80% and 95% Prediction Intervals"),
  file.path(figures_dir, "baseline_prediction_intervals.png"),
  width = 10,
  height = 9
)

if (interactive()) {
  for (model_name in names(model_functions)) {
    residuals <- baseline_forecasts |>
      filter(sample == "train", model == model_name) |>
      transmute(residual = actual - point_forecast) |>
      pull(residual)

    message("\nResidual diagnostics plots: ", model_name)
    forecast::checkresiduals(ts(stats::na.omit(residuals)))
  }
}
