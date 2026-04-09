source(if (file.exists("scripts/utils.R")) "scripts/utils.R" else "utils.R")

paths <- setup_analysis(c("forecast", "readr", "dplyr", "ggplot2"))
processed_dir <- paths$processed_dir
figures_dir <- paths$figures_dir

split_data <- load_split_data(processed_dir)
train_data <- split_data$train
holdout_data <- split_data$holdout

build_feature_pool <- function(df) {
  df |>
    arrange(date) |>
    mutate(
      usd_level_lag2 = lag(usd_broad_index_lag1, 1),
      usd_level_lag5 = lag(usd_broad_index_lag1, 4),
      sp500_level_lag2 = lag(sp500_index_lag1, 1),
      sp500_level_lag5 = lag(sp500_index_lag1, 4),
      wti_level_lag2 = lag(wti_price_lag1, 1),
      wti_level_lag5 = lag(wti_price_lag1, 4),
      treasury_level_lag2 = lag(treasury_10y_yield_lag1, 1),
      treasury_level_lag5 = lag(treasury_10y_yield_lag1, 4),
      usd_change_1d = usd_broad_index_lag1 - lag(usd_broad_index_lag1),
      sp500_change_1d = sp500_index_lag1 - lag(sp500_index_lag1),
      wti_change_1d = wti_price_lag1 - lag(wti_price_lag1),
      treasury_change_1d = treasury_10y_yield_lag1 - lag(treasury_10y_yield_lag1),
      usd_change_5d = usd_broad_index_lag1 - lag(usd_broad_index_lag1, 5),
      sp500_change_5d = sp500_index_lag1 - lag(sp500_index_lag1, 5),
      wti_change_5d = wti_price_lag1 - lag(wti_price_lag1, 5),
      treasury_change_5d = treasury_10y_yield_lag1 - lag(treasury_10y_yield_lag1, 5)
    )
}

feature_candidates <- c(
  "usd_broad_index_lag1",
  "usd_level_lag2",
  "usd_level_lag5",
  "sp500_index_lag1",
  "sp500_level_lag2",
  "sp500_level_lag5",
  "wti_price_lag1",
  "wti_level_lag2",
  "wti_level_lag5",
  "treasury_10y_yield_lag1",
  "treasury_level_lag2",
  "treasury_level_lag5",
  "usd_change_1d",
  "sp500_change_1d",
  "wti_change_1d",
  "treasury_change_1d",
  "usd_change_5d",
  "sp500_change_5d",
  "wti_change_5d",
  "treasury_change_5d"
)

model_data <- bind_rows(
  train_data |> mutate(sample = "train"),
  holdout_data |> mutate(sample = "holdout")
) |>
  build_feature_pool() |>
  filter(if_all(all_of(c("gold_close", "gold_log_return", feature_candidates)), ~ !is.na(.x)))

train_model_data <- model_data |> filter(sample == "train")
holdout_model_data <- model_data |> filter(sample == "holdout")
train_y <- train_model_data$gold_close
holdout_y <- holdout_model_data$gold_close

feature_screening <- tibble(
  feature = feature_candidates,
  corr_with_gold_log_return = vapply(
    feature_candidates,
    function(feature_name) cor(train_model_data$gold_log_return, train_model_data[[feature_name]], use = "complete.obs"),
    numeric(1)
  )
) |>
  mutate(
    abs_corr = abs(corr_with_gold_log_return),
    feature_group = case_when(
      grepl("^usd", feature) ~ "usd",
      grepl("^sp500", feature) ~ "sp500",
      grepl("^wti", feature) ~ "wti",
      TRUE ~ "treasury"
    ),
    feature_type = if_else(grepl("change", feature), "change", "level")
  ) |>
  arrange(desc(abs_corr))

screened_features <- feature_screening |>
  group_by(feature_group) |>
  slice_max(abs_corr, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(abs_corr))

predictor_sets <- list(
  arimax_market_core_lag1 = c("usd_broad_index_lag1", "sp500_index_lag1", "wti_price_lag1"),
  arimax_screened_top2 = screened_features$feature[1:2],
  arimax_screened_top3 = screened_features$feature[1:3],
  arimax_screened_top4 = screened_features$feature[1:4]
)

print_section(
  "ARIMAX scope decisions",
  tibble(
    decision = c(
      "Use regression with ARIMA errors",
      "Add transformed predictors",
      "Use a small lag grid",
      "Keep the candidate models sparse"
    ),
    reason = c(
      "This combines external predictors with ARIMA-style errors.",
      "Daily changes are more relevant than raw trending levels for next-day forecasting.",
      "Lag 1, lag 2, and lag 5 are enough for this project.",
      "This reduces the risk of trend-based overfitting."
    )
  )
)
print_section("ARIMAX screened features", feature_screening, n = nrow(feature_screening))
print_section(
  "ARIMAX predictor sets",
  bind_rows(lapply(names(predictor_sets), function(model_name) {
    tibble(model = model_name, predictors = paste(predictor_sets[[model_name]], collapse = ", "))
  })),
  n = length(predictor_sets)
)

show_and_save_plot(
  feature_screening |>
    slice_head(n = 10) |>
    mutate(feature = reorder(feature, abs_corr)) |>
    ggplot(aes(x = feature, y = abs_corr, fill = feature_type)) +
    geom_col() +
    coord_flip() +
    labs(title = "Top Screened ARIMAX Features", x = NULL, y = "Absolute correlation", fill = NULL) +
    theme_minimal(),
  file.path(figures_dir, "arimax_feature_screening.png"),
  width = 9,
  height = 6
)

make_xreg_matrix <- function(df, predictors) as.matrix(df[, predictors, drop = FALSE])

initial_fits <- lapply(names(predictor_sets), function(model_name) {
  predictors <- predictor_sets[[model_name]]
  fit <- auto.arima(
    ts(train_y),
    xreg = make_xreg_matrix(train_model_data, predictors),
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE
  )

  order <- arimaorder(fit)

  list(
    model = model_name,
    predictors = predictors,
    method = paste0("Regression with ARIMA(", order[1], ",", order[2], ",", order[3], ") errors"),
    fit = fit
  )
})

candidate_specs <- bind_rows(lapply(initial_fits, function(item) {
  order <- arimaorder(item$fit)

  tibble(
    model = item$model,
    predictors = paste(item$predictors, collapse = ", "),
    p = order[1],
    d = order[2],
    q = order[3],
    include_drift = "drift" %in% names(item$fit$coef),
    method = item$method,
    aicc = item$fit$aicc
  )
}))

training_metrics <- bind_rows(lapply(initial_fits, function(item) {
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

arimax_residual_diagnostics <- bind_rows(lapply(initial_fits, function(item) {
  summarise_residuals(as.numeric(stats::na.omit(residuals(item$fit))), fitdf = length(item$fit$coef)) |>
    mutate(model = item$model, method = item$method, aicc = item$fit$aicc, .before = residual_mean)
}))

rolling_holdout_forecasts <- function(item) {
  point_forecast <- lo80 <- hi80 <- lo95 <- hi95 <- rep(NA_real_, nrow(holdout_model_data))

  for (i in seq_len(nrow(holdout_model_data))) {
    history_data <- bind_rows(train_model_data, head(holdout_model_data, i - 1))
    fit <- tryCatch(
      Arima(
        ts(history_data$gold_close),
        xreg = make_xreg_matrix(history_data, item$predictors),
        model = item$fit
      ),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      next
    }

    forecast_object <- forecast(
      fit,
      xreg = make_xreg_matrix(holdout_model_data[i, , drop = FALSE], item$predictors),
      h = 1,
      level = c(80, 95)
    )

    point_forecast[i] <- as.numeric(forecast_object$mean[1])
    lo80[i] <- as.numeric(forecast_object$lower[1, "80%"])
    hi80[i] <- as.numeric(forecast_object$upper[1, "80%"])
    lo95[i] <- as.numeric(forecast_object$lower[1, "95%"])
    hi95[i] <- as.numeric(forecast_object$upper[1, "95%"])
  }

  tibble(
    date = holdout_model_data$date,
    model = item$model,
    method = item$method,
    actual = holdout_y,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )
}

arimax_holdout_forecasts <- bind_rows(lapply(initial_fits, rolling_holdout_forecasts))

holdout_metrics <- summarise_forecasts(arimax_holdout_forecasts, c("model", "method")) |>
  left_join(candidate_specs |> select(model, aicc), by = "model") |>
  mutate(sample = "holdout", .before = model) |>
  select(sample, model, method, rmse, n_predictions, coverage80, coverage95, avg_width80, avg_width95, aicc)

arimax_metrics <- bind_rows(training_metrics, holdout_metrics)

write_csv(arimax_metrics, file.path(processed_dir, "arimax_metrics.csv"))
write_csv(arimax_residual_diagnostics, file.path(processed_dir, "arimax_residual_diagnostics.csv"))
write_csv(arimax_holdout_forecasts, file.path(processed_dir, "arimax_holdout_forecasts.csv"))

print_section("ARIMAX candidate models", candidate_specs, n = nrow(candidate_specs))
print_section("ARIMAX metrics", format_numeric_table(arimax_metrics), n = nrow(arimax_metrics))
print_section(
  "ARIMAX residual diagnostics",
  format_numeric_table(arimax_residual_diagnostics),
  n = nrow(arimax_residual_diagnostics)
)

show_and_save_plot(
  make_holdout_plot(
    train_model_data,
    holdout_model_data,
    arimax_holdout_forecasts,
    "Dynamic Regression Forecasts on the Holdout Period"
  ),
  file.path(figures_dir, "arimax_holdout_forecasts.png"),
  width = 10,
  height = 6
)

best_arimax_model <- holdout_metrics |> arrange(rmse) |> slice(1) |> pull(model)
best_arimax_method <- holdout_metrics |> filter(model == best_arimax_model) |> pull(method)

show_and_save_plot(
  make_interval_plot(
    arimax_holdout_forecasts |> filter(model == best_arimax_model),
    paste("Best Dynamic Regression Model:", best_arimax_model),
    best_arimax_method
  ),
  file.path(figures_dir, "best_arimax_prediction_intervals.png"),
  width = 10,
  height = 6
)

if (interactive()) {
  for (item in initial_fits) {
    message("\nResidual diagnostics plots: ", item$model)
    forecast::checkresiduals(item$fit)
  }
}
