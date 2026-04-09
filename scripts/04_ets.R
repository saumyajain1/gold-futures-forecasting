source(if (file.exists("scripts/utils.R")) "scripts/utils.R" else "utils.R")

paths <- setup_analysis(c("forecast", "readr", "dplyr", "ggplot2"))
processed_dir <- paths$processed_dir
figures_dir <- paths$figures_dir

split_data <- load_split_data(processed_dir)
train_data <- split_data$train
holdout_data <- split_data$holdout
train_y <- train_data$gold_close
holdout_y <- holdout_data$gold_close

print_section(
  "ETS scope decision",
  tibble(
    decision = "Do not fit Holt-Winters seasonal ETS",
    reason = "The trading-day calendar is irregular and the seasonality signal is weak."
  )
)

ets_functions <- list(
  ses = function(y, h) ses(ts(y), h = h, level = c(80, 95)),
  holt = function(y, h) holt(ts(y), h = h, level = c(80, 95)),
  damped_holt = function(y, h) holt(ts(y), h = h, damped = TRUE, level = c(80, 95)),
  ets_auto = function(y, h) forecast(ets(ts(y), model = "ZZN", restrict = TRUE), h = h, level = c(80, 95))
)

training_fits <- lapply(ets_functions, function(fit_fun) fit_fun(train_y, nrow(holdout_data)))

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
  summarise_residuals(as.numeric(stats::na.omit(residuals(fit)))) |>
    mutate(model = model_name, method = fit$model$method, aicc = fit$model$aicc, .before = residual_mean)
}))

rolling_holdout_forecasts <- function(model_name) {
  point_forecast <- lo80 <- hi80 <- lo95 <- hi95 <- rep(NA_real_, nrow(holdout_data))

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

holdout_metrics <- summarise_forecasts(ets_holdout_forecasts, "model") |>
  left_join(training_metrics |> select(model, method, aicc), by = "model") |>
  mutate(sample = "holdout", .before = model) |>
  select(sample, model, method, rmse, n_predictions, coverage80, coverage95, avg_width80, avg_width95, aicc)

ets_metrics <- bind_rows(training_metrics, holdout_metrics)

write_csv(ets_metrics, file.path(processed_dir, "ets_metrics.csv"))
write_csv(ets_residual_diagnostics, file.path(processed_dir, "ets_residual_diagnostics.csv"))
write_csv(ets_holdout_forecasts, file.path(processed_dir, "ets_holdout_forecasts.csv"))

print_section("ETS metrics", format_numeric_table(ets_metrics), n = nrow(ets_metrics))
print_section(
  "ETS residual diagnostics",
  format_numeric_table(ets_residual_diagnostics),
  n = nrow(ets_residual_diagnostics)
)

show_and_save_plot(
  make_holdout_plot(
    train_data,
    holdout_data,
    ets_holdout_forecasts,
    "ETS Forecasts on the Holdout Period",
    colors = c(
      actual = "black",
      ses = "steelblue",
      holt = "darkgreen",
      damped_holt = "firebrick",
      ets_auto = "darkorange"
    )
  ),
  file.path(figures_dir, "ets_holdout_forecasts.png"),
  width = 10,
  height = 6
)

best_ets_model <- holdout_metrics |> arrange(rmse) |> slice(1) |> pull(model)
best_ets_subtitle <- training_fits[[best_ets_model]]$model$method

show_and_save_plot(
  make_interval_plot(
    ets_holdout_forecasts |> filter(model == best_ets_model),
    paste("Best ETS Model:", best_ets_model),
    paste("Training fit:", best_ets_subtitle)
  ),
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
