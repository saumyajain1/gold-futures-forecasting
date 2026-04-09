source(if (file.exists("scripts/utils.R")) "scripts/utils.R" else "utils.R")

paths <- setup_analysis(c("readr", "dplyr", "ggplot2", "tidyr", "forcats", "scales"))
processed_dir <- paths$processed_dir
figures_dir <- paths$figures_dir
cleanup_intermediate_files <- TRUE

predictors <- c(
  "usd_broad_index_lag1",
  "sp500_index_lag1",
  "wti_price_lag1",
  "treasury_10y_yield_lag1"
)

save_report <- function(file_name, data) {
  write_csv(data, file.path(processed_dir, file_name))
}

baseline_method <- function(model) {
  recode(model, mean = "Mean benchmark", naive = "Naive benchmark")
}

datasets <- read_processed_files(
  processed_dir,
  c(
    gold_macro_data = "gold_macro_data.csv",
    train = "train_data.csv",
    holdout = "holdout_data.csv"
  )
)

metrics <- read_processed_files(
  processed_dir,
  c(
    baseline = "baseline_metrics.csv",
    ets = "ets_metrics.csv",
    arima = "arima_metrics.csv",
    arimax = "arimax_metrics.csv"
  )
)

diagnostics <- read_processed_files(
  processed_dir,
  c(
    baseline = "baseline_residual_diagnostics.csv",
    ets = "ets_residual_diagnostics.csv",
    arima = "arima_residual_diagnostics.csv",
    arimax = "arimax_residual_diagnostics.csv"
  )
)

forecasts <- read_processed_files(
  processed_dir,
  c(
    baseline = "baseline_holdout_forecasts.csv",
    ets = "ets_holdout_forecasts.csv",
    arima = "arima_holdout_forecasts.csv",
    arimax = "arimax_holdout_forecasts.csv"
  )
)

gold_macro_data <- datasets$gold_macro_data
train_data <- datasets$train
holdout_data <- datasets$holdout
split_date <- max(train_data$date)

final_variable_summary <- bind_rows(
  make_summary_table(gold_macro_data, "full"),
  make_summary_table(train_data, "train"),
  make_summary_table(holdout_data, "holdout")
) |>
  round_numeric_table()

save_report("final_variable_summary.csv", final_variable_summary)

show_and_save_plot(
  make_time_plot(
    gold_macro_data,
    c("gold_close", "gold_log_return"),
    "Gold Series with Train-Holdout Split",
    split_date,
    "steelblue"
  ),
  file.path(figures_dir, "final_gold_series_split.png"),
  width = 9,
  height = 7
)

show_and_save_plot(
  make_time_plot(
    gold_macro_data,
    predictors,
    "Lagged Macro-Financial Predictors",
    split_date,
    "darkgreen"
  ),
  file.path(figures_dir, "final_macro_predictors_split.png"),
  width = 9,
  height = 9
)

weekday_plot_data <- train_data |>
  mutate(
    weekday = factor(
      weekdays(date, abbreviate = TRUE),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri")
    )
  )

show_and_save_plot(
  ggplot(weekday_plot_data, aes(x = weekday, y = gold_log_return)) +
    geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.2) +
    labs(title = "Gold Log Return by Weekday (Training Set)", x = NULL, y = "Gold log return") +
    theme_minimal(),
  file.path(figures_dir, "final_weekday_seasonality.png"),
  width = 8,
  height = 5
)

save_base_plot(
  file.path(figures_dir, "final_gold_acf_pacf.png"),
  code = {
    par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
    acf(train_data$gold_close, main = "ACF of Gold Close (Training Set)")
    pacf(train_data$gold_close, main = "PACF of Gold Close (Training Set)")
    acf(train_data$gold_log_return, main = "ACF of Gold Log Return (Training Set)")
    pacf(train_data$gold_log_return, main = "PACF of Gold Log Return (Training Set)")
  }
)

show_base_plot(
  {
    par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
    acf(train_data$gold_close, main = "ACF of Gold Close (Training Set)")
    pacf(train_data$gold_close, main = "PACF of Gold Close (Training Set)")
    acf(train_data$gold_log_return, main = "ACF of Gold Log Return (Training Set)")
    pacf(train_data$gold_log_return, main = "PACF of Gold Log Return (Training Set)")
  }
)

all_model_metrics <- bind_rows(
  metrics$baseline |> mutate(model_class = "baseline", method = baseline_method(model)),
  metrics$ets |> mutate(model_class = "ets"),
  metrics$arima |> mutate(model_class = "arima"),
  metrics$arimax |> mutate(model_class = "arimax")
) |>
  add_model_labels()

holdout_ranking <- all_model_metrics |>
  filter(sample == "holdout") |>
  arrange(rmse) |>
  mutate(
    rank = row_number(),
    rmse_diff_from_naive = rmse - rmse[model == "naive"][1],
    coverage80_gap = coverage80 - 0.80,
    coverage95_gap = coverage95 - 0.95
  )

class_winners <- holdout_ranking |>
  group_by(model_class, model_class_label) |>
  slice_min(rmse, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(match(model_class, c("baseline", "ets", "arima", "arimax")))

winner_generalization_summary <- class_winners |>
  select(
    model_class,
    display_label,
    method,
    model,
    rmse_holdout = rmse,
    coverage80,
    coverage95,
    avg_width80,
    avg_width95
  ) |>
  left_join(
    all_model_metrics |>
      filter(sample == "train") |>
      select(model_class, model, rmse_train = rmse),
    by = c("model_class", "model")
  ) |>
  mutate(rmse_gap = rmse_holdout - rmse_train) |>
  select(
    model_class,
    display_label,
    method,
    rmse_train,
    rmse_holdout,
    rmse_gap,
    coverage80,
    coverage95,
    avg_width80,
    avg_width95
  )

winner_residual_diagnostics <- bind_rows(
  diagnostics$baseline |>
    mutate(model_class = "baseline", method = baseline_method(model)),
  diagnostics$ets |>
    mutate(model_class = "ets"),
  diagnostics$arima |>
    mutate(model_class = "arima"),
  diagnostics$arimax |>
    mutate(model_class = "arimax")
) |>
  inner_join(class_winners |> select(model_class, model, display_label), by = c("model_class", "model")) |>
  mutate(model_class = label_values(model_class, model_class_label_map())) |>
  transmute(
    model_class,
    display_label,
    method,
    residual_mean = round(residual_mean, 3),
    residual_sd = round(residual_sd, 3),
    residual_acf1 = round(residual_acf1, 3),
    ljung_box_pvalue = format_pvalue(ljung_box_pvalue)
  )

selection_summary <- bind_rows(
  holdout_ranking |>
    slice_min(rmse, n = 1, with_ties = FALSE) |>
    transmute(criterion = "Lowest holdout RMSE overall", winner = display_label, value = sprintf("%.3f", rmse)),
  holdout_ranking |>
    filter(model_class != "baseline") |>
    slice_min(rmse, n = 1, with_ties = FALSE) |>
    transmute(criterion = "Lowest holdout RMSE among non-baselines", winner = display_label, value = sprintf("%.3f", rmse)),
  class_winners |>
    slice_max(coverage80, n = 1, with_ties = FALSE) |>
    transmute(criterion = "Best 80% interval coverage among finalists", winner = display_label, value = sprintf("%.3f", coverage80)),
  class_winners |>
    slice_max(coverage95, n = 1, with_ties = FALSE) |>
    transmute(criterion = "Best 95% interval coverage among finalists", winner = display_label, value = sprintf("%.3f", coverage95))
)

shortlist_forecasts <- bind_rows(
  forecasts$baseline |> filter(model == "naive") |> mutate(model_class = "baseline"),
  forecasts$ets |> mutate(model_class = "ets"),
  forecasts$arima |> mutate(model_class = "arima"),
  forecasts$arimax |> mutate(model_class = "arimax")
) |>
  inner_join(
    class_winners |> select(model_class, model, display_label, model_class_label),
    by = c("model_class", "model")
  )

holdout_ranking_report <- holdout_ranking |>
  select(
    rank,
    model_class_label,
    display_label,
    method,
    rmse,
    rmse_diff_from_naive,
    coverage80,
    coverage95,
    avg_width80,
    avg_width95,
    aicc
  ) |>
  round_numeric_table()

class_winners_report <- class_winners |>
  select(
    model_class_label,
    display_label,
    method,
    rmse,
    rmse_diff_from_naive,
    coverage80,
    coverage95,
    avg_width80,
    avg_width95,
    aicc
  ) |>
  round_numeric_table()

winner_generalization_report <- winner_generalization_summary |>
  mutate(model_class = label_values(model_class, model_class_label_map())) |>
  round_numeric_table()

save_report("final_model_ranking_holdout.csv", holdout_ranking_report)
save_report("final_class_winners.csv", class_winners_report)
save_report("final_winner_generalization_summary.csv", winner_generalization_report)
save_report("final_winner_residual_diagnostics.csv", winner_residual_diagnostics)
save_report("final_selection_summary.csv", selection_summary)

print_section("Final variable summary", final_variable_summary, n = nrow(final_variable_summary))
print_section("Final holdout ranking", holdout_ranking_report, n = nrow(holdout_ranking_report))
print_section("Final class winners", class_winners_report, n = nrow(class_winners_report))
print_section(
  "Winner train vs holdout summary",
  winner_generalization_report,
  n = nrow(winner_generalization_report)
)
print_section(
  "Winner residual diagnostics",
  winner_residual_diagnostics,
  n = nrow(winner_residual_diagnostics)
)
print_section("Final selection summary", selection_summary, n = nrow(selection_summary))

class_palette <- c(Baseline = "#4C566A", ETS = "#1F78B4", ARIMA = "#33A02C", ARIMAX = "#E31A1C")
shortlist_palette <- c(
  "Naive" = "#4C566A",
  "ETS(M,N,N)" = "#1F78B4",
  "ARIMA(1,1,1) + drift" = "#33A02C",
  "ARIMAX market core (lag 1)" = "#E31A1C"
)

show_and_save_plot(
  holdout_ranking |>
    mutate(display_label = fct_reorder(display_label, rmse)) |>
    ggplot(aes(x = rmse, y = display_label, fill = model_class_label)) +
    geom_col() +
    geom_vline(
      xintercept = class_winners |> filter(model_class == "baseline") |> pull(rmse),
      linetype = "dashed",
      color = "grey35"
    ) +
    geom_text(aes(label = sprintf("%.3f", rmse)), hjust = -0.1, size = 3) +
    scale_fill_manual(values = class_palette) +
    scale_x_continuous(labels = label_number(accuracy = 0.001), expand = expansion(mult = c(0, 0.12))) +
    labs(
      title = "Holdout RMSE Ranking Across All Models",
      subtitle = "Dashed line marks the naive benchmark",
      x = "Holdout RMSE",
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(),
  file.path(figures_dir, "final_holdout_rmse_ranking.png"),
  width = 10,
  height = 7
)

holdout_actual <- holdout_data |> transmute(date = as.Date(date), actual = gold_close)
zoom_start <- holdout_actual |> slice_tail(n = 120) |> summarise(date = min(date)) |> pull(date)

show_and_save_plot(
  ggplot() +
    geom_line(data = holdout_actual, aes(x = date, y = actual), color = "black", linewidth = 0.6) +
    geom_line(
      data = shortlist_forecasts,
      aes(x = date, y = point_forecast, color = display_label),
      linewidth = 0.45
    ) +
    scale_color_manual(values = shortlist_palette) +
    labs(
      title = "Finalist Forecasts Across the Holdout Period",
      subtitle = "Black line shows actual gold close",
      x = "Date",
      y = "Gold close",
      color = NULL
    ) +
    theme_minimal(),
  file.path(figures_dir, "final_shortlist_holdout_overlay.png"),
  width = 10,
  height = 6
)

show_and_save_plot(
  ggplot() +
    geom_line(
      data = holdout_actual |> filter(date >= zoom_start),
      aes(x = date, y = actual),
      color = "black",
      linewidth = 0.7
    ) +
    geom_line(
      data = shortlist_forecasts |> filter(date >= zoom_start),
      aes(x = date, y = point_forecast, color = display_label),
      linewidth = 0.5
    ) +
    scale_color_manual(values = shortlist_palette) +
    labs(title = "Finalist Forecasts: Last 120 Holdout Observations", x = "Date", y = "Gold close", color = NULL) +
    theme_minimal(),
  file.path(figures_dir, "final_shortlist_holdout_zoom.png"),
  width = 10,
  height = 6
)

show_and_save_plot(
  class_winners |>
    select(display_label, coverage80, coverage95) |>
    pivot_longer(c(coverage80, coverage95), names_to = "interval", values_to = "coverage") |>
    mutate(
      interval = recode(interval, coverage80 = "80% interval", coverage95 = "95% interval"),
      nominal = if_else(interval == "80% interval", 0.80, 0.95)
    ) |>
    ggplot(aes(x = fct_reorder(display_label, coverage), y = coverage, color = display_label)) +
    geom_hline(aes(yintercept = nominal), linetype = "dashed", color = "grey40") +
    geom_point(size = 3) +
    coord_flip() +
    facet_wrap(~ interval, nrow = 1) +
    scale_color_manual(values = shortlist_palette) +
    scale_y_continuous(labels = label_percent(accuracy = 1), limits = c(0, 1)) +
    labs(
      title = "Prediction Interval Coverage for the Finalists",
      subtitle = "Dashed lines show nominal coverage targets",
      x = NULL,
      y = "Observed coverage",
      color = NULL
    ) +
    theme_minimal(),
  file.path(figures_dir, "final_shortlist_interval_coverage.png"),
  width = 10,
  height = 6
)

if (cleanup_intermediate_files) {
  unlink(file.path(
    processed_dir,
    c(
      "baseline_metrics.csv",
      "baseline_residual_diagnostics.csv",
      "baseline_holdout_forecasts.csv",
      "ets_metrics.csv",
      "ets_residual_diagnostics.csv",
      "ets_holdout_forecasts.csv",
      "arima_metrics.csv",
      "arima_residual_diagnostics.csv",
      "arima_holdout_forecasts.csv",
      "arimax_metrics.csv",
      "arimax_residual_diagnostics.csv",
      "arimax_holdout_forecasts.csv"
    )
  ))
}

message(
  "\nSaved final summary tables to ", processed_dir,
  "\nSaved report-ready plots to ", figures_dir,
  "\nRemoved intermediate CSVs: ", cleanup_intermediate_files
)
