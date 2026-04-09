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

arima_metrics <- read_csv(
  file.path(processed_dir, "arima_metrics.csv"),
  show_col_types = FALSE
)

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

feature_sample_summary <- tibble(
  sample = c("train", "holdout"),
  rows = c(nrow(train_model_data), nrow(holdout_model_data)),
  start_date = c(min(train_model_data$date), min(holdout_model_data$date)),
  end_date = c(max(train_model_data$date), max(holdout_model_data$date))
)

feature_screening <- tibble(
  feature = feature_candidates,
  corr_with_gold_log_return = vapply(
    feature_candidates,
    function(feature_name) {
      cor(
        train_model_data$gold_log_return,
        train_model_data[[feature_name]],
        use = "complete.obs"
      )
    },
    numeric(1)
  )
) |>
  mutate(
    abs_corr = abs(corr_with_gold_log_return),
    feature_group = case_when(
      grepl("^usd", feature) ~ "usd",
      grepl("^sp500", feature) ~ "sp500",
      grepl("^wti", feature) ~ "wti",
      grepl("^treasury", feature) ~ "treasury"
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
  arimax_market_core_lag1 = c(
    "usd_broad_index_lag1",
    "sp500_index_lag1",
    "wti_price_lag1"
  ),
  arimax_screened_top2 = screened_features$feature[1:2],
  arimax_screened_top3 = screened_features$feature[1:3],
  arimax_screened_top4 = screened_features$feature[1:4]
)

scope_decision <- tibble(
  decision = c(
    "Use regression with ARIMA errors",
    "Add transformed predictor features",
    "Try a small lag grid for macro predictors",
    "Keep candidate models sparse and screened on the training set",
    "Keep dynamic-regression coefficients fixed from the training fit"
  ),
  reason = c(
    "This combines external predictors with ARIMA-style autocorrelated errors.",
    "Daily changes are more aligned with forecasting next-day gold movements than raw trending levels.",
    "Lag 1, lag 2, and lag 5 features are a small realistic grid for this project.",
    "This avoids loading weak predictors into the regression just because they share long-run trend with gold.",
    "Moving 1-step holdout forecasts update the model state while retaining the training-set parameter estimates."
  )
)

predictor_summary <- bind_rows(lapply(names(predictor_sets), function(model_name) {
  tibble(
    model = model_name,
    predictors = paste(predictor_sets[[model_name]], collapse = ", ")
  )
}))

print_section("ARIMAX scope decisions", scope_decision, n = nrow(scope_decision))
print_section("ARIMAX modeling sample", feature_sample_summary, n = nrow(feature_sample_summary))
print_section("ARIMAX feature screening", feature_screening, n = nrow(feature_screening))
print_section("ARIMAX screened feature choices", screened_features, n = nrow(screened_features))
print_section("ARIMAX predictor sets", predictor_summary, n = nrow(predictor_summary))

screening_plot <- feature_screening |>
  slice_head(n = 10) |>
  mutate(feature = reorder(feature, abs_corr)) |>
  ggplot(aes(x = feature, y = abs_corr, fill = feature_type)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top Screened ARIMAX Features",
    x = NULL,
    y = "Absolute correlation with gold log return",
    fill = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  screening_plot,
  file.path(figures_dir, "arimax_feature_screening.png"),
  width = 9,
  height = 6
)

train_y <- train_model_data$gold_close
holdout_y <- holdout_model_data$gold_close

make_xreg_matrix <- function(df, predictors) {
  as.matrix(df[, predictors, drop = FALSE])
}

fit_dynamic_auto <- function(y, xreg) {
  auto.arima(
    ts(y),
    xreg = xreg,
    seasonal = FALSE,
    stepwise = FALSE,
    approximation = FALSE
  )
}

initial_fits <- lapply(names(predictor_sets), function(model_name) {
  predictors <- predictor_sets[[model_name]]
  xreg_train <- make_xreg_matrix(train_model_data, predictors)
  fit <- fit_dynamic_auto(train_y, xreg_train)
  order <- arimaorder(fit)
  method <- paste0(
    "Regression with ARIMA(",
    order[1], ",", order[2], ",", order[3], ") errors"
  )

  list(
    model = model_name,
    predictors = predictors,
    method = method,
    order = order,
    include_drift = "drift" %in% names(fit$coef),
    fit = fit
  )
})

candidate_specs <- bind_rows(lapply(initial_fits, function(item) {
  tibble(
    model = item$model,
    predictors = paste(item$predictors, collapse = ", "),
    p = item$order[1],
    d = item$order[2],
    q = item$order[3],
    include_drift = item$include_drift,
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

coefficient_table <- bind_rows(lapply(initial_fits, function(item) {
  estimates <- coef(item$fit)
  standard_errors <- sqrt(diag(item$fit$var.coef))

  tibble(
    model = item$model,
    method = item$method,
    term = names(estimates),
    estimate = as.numeric(estimates),
    std_error = as.numeric(standard_errors)
  )
}))

arimax_residual_diagnostics <- bind_rows(lapply(initial_fits, function(item) {
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
      fitdf = length(item$fit$coef)
    )$p.value
  )
}))

rolling_holdout_forecasts <- function(item) {
  point_forecast <- rep(NA_real_, nrow(holdout_model_data))
  lo80 <- rep(NA_real_, nrow(holdout_model_data))
  hi80 <- rep(NA_real_, nrow(holdout_model_data))
  lo95 <- rep(NA_real_, nrow(holdout_model_data))
  hi95 <- rep(NA_real_, nrow(holdout_model_data))

  for (i in seq_len(nrow(holdout_model_data))) {
    history_data <- bind_rows(train_model_data, head(holdout_model_data, i - 1))
    history_y <- history_data$gold_close
    history_xreg <- make_xreg_matrix(history_data, item$predictors)
    new_xreg <- make_xreg_matrix(holdout_model_data[i, , drop = FALSE], item$predictors)

    fit <- tryCatch(
      Arima(ts(history_y), xreg = history_xreg, model = item$fit),
      error = function(e) NULL
    )

    if (is.null(fit)) {
      next
    }

    fc <- forecast(fit, xreg = new_xreg, h = 1, level = c(80, 95))

    point_forecast[i] <- as.numeric(fc$mean[1])
    lo80[i] <- as.numeric(fc$lower[1, "80%"])
    hi80[i] <- as.numeric(fc$upper[1, "80%"])
    lo95[i] <- as.numeric(fc$lower[1, "95%"])
    hi95[i] <- as.numeric(fc$upper[1, "95%"])
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

holdout_metrics <- arimax_holdout_forecasts |>
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
  left_join(candidate_specs |> select(model, aicc), by = "model") |>
  mutate(sample = "holdout") |>
  select(sample, model, method, rmse, n_predictions, coverage80, coverage95, avg_width80, avg_width95, aicc)

arimax_metrics <- bind_rows(training_metrics, holdout_metrics)

best_ets <- ets_metrics |>
  filter(sample == "holdout") |>
  arrange(rmse) |>
  slice(1) |>
  mutate(model_class = "ets")

best_arima <- arima_metrics |>
  filter(sample == "holdout") |>
  arrange(rmse) |>
  slice(1) |>
  mutate(model_class = "arima")

naive_baseline <- baseline_metrics |>
  filter(sample == "holdout", model == "naive") |>
  mutate(method = "Naive baseline", aicc = NA_real_, model_class = "baseline")

arimax_vs_existing <- bind_rows(
  naive_baseline,
  best_ets,
  best_arima,
  holdout_metrics |> mutate(model_class = "arimax")
) |>
  select(model_class, sample, model, method, rmse, coverage80, coverage95, avg_width80, avg_width95, aicc) |>
  arrange(rmse)

arimax_metrics_display <- arimax_metrics |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
      ~ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))
    )
  )

arimax_residual_diagnostics_display <- arimax_residual_diagnostics |>
  mutate(across(c(aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue), ~sprintf("%.3f", .x)))

arimax_vs_existing_display <- arimax_vs_existing |>
  mutate(
    across(
      c(rmse, coverage80, coverage95, avg_width80, avg_width95, aicc),
      ~ifelse(is.na(.x), NA_character_, sprintf("%.3f", .x))
    )
  )

write_csv(feature_screening, file.path(processed_dir, "arimax_feature_screening.csv"))
write_csv(candidate_specs, file.path(processed_dir, "arimax_candidate_models.csv"))
write_csv(coefficient_table, file.path(processed_dir, "arimax_coefficients.csv"))
write_csv(arimax_metrics, file.path(processed_dir, "arimax_metrics.csv"))
write_csv(
  arimax_residual_diagnostics,
  file.path(processed_dir, "arimax_residual_diagnostics.csv")
)
write_csv(
  arimax_holdout_forecasts,
  file.path(processed_dir, "arimax_holdout_forecasts.csv")
)
write_csv(
  arimax_vs_existing,
  file.path(processed_dir, "arimax_vs_existing.csv")
)

print_section("ARIMAX candidate models", candidate_specs, n = nrow(candidate_specs))
print_section("ARIMAX metrics", arimax_metrics_display, n = nrow(arimax_metrics_display))
print_section(
  "ARIMAX residual diagnostics",
  arimax_residual_diagnostics_display,
  n = nrow(arimax_residual_diagnostics_display)
)
print_section(
  "ARIMAX vs existing benchmarks",
  arimax_vs_existing_display,
  n = nrow(arimax_vs_existing_display)
)

arimax_plot_data <- bind_rows(
  tail(train_model_data, 120) |>
    transmute(date, series = "actual", value = gold_close),
  holdout_model_data |>
    transmute(date, series = "actual", value = gold_close),
  arimax_holdout_forecasts |>
    transmute(date, series = model, value = point_forecast)
)

arimax_plot <- ggplot(arimax_plot_data, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.5) +
  geom_vline(xintercept = max(train_model_data$date), linetype = "dashed", color = "grey40") +
  labs(
    title = "Dynamic Regression Forecasts on the Holdout Period",
    x = "Date",
    y = "Gold close",
    color = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  arimax_plot,
  file.path(figures_dir, "arimax_holdout_forecasts.png"),
  width = 10,
  height = 6
)

best_arimax_model <- holdout_metrics |>
  arrange(rmse) |>
  slice(1) |>
  pull(model)

best_arimax_holdout <- arimax_holdout_forecasts |>
  filter(model == best_arimax_model)

best_arimax_method <- holdout_metrics |>
  filter(model == best_arimax_model) |>
  pull(method)

best_arimax_plot <- ggplot(best_arimax_holdout, aes(x = date, y = point_forecast)) +
  geom_ribbon(aes(ymin = lo95, ymax = hi95), fill = "grey85") +
  geom_ribbon(aes(ymin = lo80, ymax = hi80), fill = "grey70") +
  geom_line(aes(y = actual), color = "black", linewidth = 0.4) +
  geom_line(color = "steelblue", linewidth = 0.4) +
  labs(
    title = paste("Best Dynamic Regression Model:", best_arimax_model),
    subtitle = best_arimax_method,
    x = "Date",
    y = "Gold close"
  ) +
  theme_minimal()

show_and_save_plot(
  best_arimax_plot,
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

message(
  "\nBest dynamic regression model on holdout RMSE: ", best_arimax_model,
  " (", best_arimax_method, ")",
  "\nSaved feature screening to ", file.path(processed_dir, "arimax_feature_screening.csv"),
  "\nSaved candidate models to ", file.path(processed_dir, "arimax_candidate_models.csv"),
  "\nSaved coefficient table to ", file.path(processed_dir, "arimax_coefficients.csv"),
  "\nSaved metrics to ", file.path(processed_dir, "arimax_metrics.csv"),
  "\nSaved residual diagnostics to ", file.path(processed_dir, "arimax_residual_diagnostics.csv"),
  "\nSaved holdout forecasts to ", file.path(processed_dir, "arimax_holdout_forecasts.csv"),
  "\nSaved comparison table to ", file.path(processed_dir, "arimax_vs_existing.csv"),
  "\nSaved plots to ", figures_dir
)
