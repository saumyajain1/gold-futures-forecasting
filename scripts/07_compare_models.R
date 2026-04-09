if (file.exists("scripts/utils.R")) {
  source("scripts/utils.R")
} else {
  source("utils.R")
}

project_root <- setup_project(c("readr", "dplyr", "ggplot2", "tidyr", "forcats", "scales"))

processed_dir <- file.path(project_root, "data", "processed")
figures_dir <- file.path(project_root, "figures")
cleanup_intermediate_files <- TRUE

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

gold_macro_data <- read_csv(
  file.path(processed_dir, "gold_macro_data.csv"),
  show_col_types = FALSE
)

train_data <- read_csv(
  file.path(processed_dir, "train_data.csv"),
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

arimax_metrics <- read_csv(
  file.path(processed_dir, "arimax_metrics.csv"),
  show_col_types = FALSE
)

baseline_residual_diagnostics <- read_csv(
  file.path(processed_dir, "baseline_residual_diagnostics.csv"),
  show_col_types = FALSE
)

ets_residual_diagnostics <- read_csv(
  file.path(processed_dir, "ets_residual_diagnostics.csv"),
  show_col_types = FALSE
)

arima_residual_diagnostics <- read_csv(
  file.path(processed_dir, "arima_residual_diagnostics.csv"),
  show_col_types = FALSE
)

arimax_residual_diagnostics <- read_csv(
  file.path(processed_dir, "arimax_residual_diagnostics.csv"),
  show_col_types = FALSE
)

baseline_holdout_forecasts <- read_csv(
  file.path(processed_dir, "baseline_holdout_forecasts.csv"),
  show_col_types = FALSE
)

ets_holdout_forecasts <- read_csv(
  file.path(processed_dir, "ets_holdout_forecasts.csv"),
  show_col_types = FALSE
)

arima_holdout_forecasts <- read_csv(
  file.path(processed_dir, "arima_holdout_forecasts.csv"),
  show_col_types = FALSE
)

arimax_holdout_forecasts <- read_csv(
  file.path(processed_dir, "arimax_holdout_forecasts.csv"),
  show_col_types = FALSE
)

holdout_data <- read_csv(
  file.path(processed_dir, "holdout_data.csv"),
  show_col_types = FALSE
)

predictors <- c(
  "usd_broad_index_lag1",
  "sp500_index_lag1",
  "wti_price_lag1",
  "treasury_10y_yield_lag1"
)

make_summary_table <- function(df, sample_name) {
  numeric_names <- setdiff(names(df), "date")

  bind_rows(lapply(numeric_names, function(var_name) {
    values <- df[[var_name]]

    tibble(
      sample = sample_name,
      variable = var_name,
      n = length(values),
      mean = mean(values),
      sd = sd(values),
      min = min(values),
      q1 = unname(quantile(values, 0.25)),
      median = median(values),
      q3 = unname(quantile(values, 0.75)),
      max = max(values)
    )
  }))
}

make_time_plot <- function(df, columns, title, split_date, color, ncol = 1) {
  df |>
    select(date, all_of(columns)) |>
    pivot_longer(-date, names_to = "series", values_to = "value") |>
    ggplot(aes(x = date, y = value)) +
    geom_line(color = color, linewidth = 0.4) +
    geom_vline(xintercept = split_date, linetype = "dashed", color = "firebrick") +
    facet_wrap(~series, ncol = ncol, scales = "free_y") +
    labs(title = title, x = "Date", y = NULL) +
    theme_minimal()
}

model_label <- function(model) {
  case_when(
    model == "mean" ~ "Mean benchmark",
    model == "naive" ~ "Naive",
    model == "ses" ~ "SES",
    model == "holt" ~ "Holt",
    model == "damped_holt" ~ "Damped Holt",
    model == "ets_auto" ~ "ETS(M,N,N)",
    model == "auto_selected" ~ "ARIMA(2,1,2) + drift",
    model == "arima_112" ~ "ARIMA(1,1,2) + drift",
    model == "arima_211" ~ "ARIMA(2,1,1) + drift",
    model == "arima_111" ~ "ARIMA(1,1,1) + drift",
    model == "arimax_market_core_lag1" ~ "ARIMAX market core (lag 1)",
    model == "arimax_screened_top2" ~ "ARIMAX screened top 2",
    model == "arimax_screened_top3" ~ "ARIMAX screened top 3",
    model == "arimax_screened_top4" ~ "ARIMAX screened top 4",
    TRUE ~ model
  )
}

model_class_label <- function(model_class) {
  case_when(
    model_class == "baseline" ~ "Baseline",
    model_class == "ets" ~ "ETS",
    model_class == "arima" ~ "ARIMA",
    model_class == "arimax" ~ "ARIMAX",
    TRUE ~ model_class
  )
}

format_pvalue <- function(x) {
  ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
}

round_report_table <- function(df, digits = 3) {
  df |>
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

split_date <- max(train_data$date)

final_variable_summary <- bind_rows(
  make_summary_table(gold_macro_data, "full"),
  make_summary_table(train_data, "train"),
  make_summary_table(holdout_data, "holdout")
) |>
  round_report_table()

write_csv(
  final_variable_summary,
  file.path(processed_dir, "final_variable_summary.csv")
)

final_gold_series_plot <- make_time_plot(
  gold_macro_data,
  c("gold_close", "gold_log_return"),
  "Gold Series with Train-Holdout Split",
  split_date,
  "steelblue"
)

show_and_save_plot(
  final_gold_series_plot,
  file.path(figures_dir, "final_gold_series_split.png"),
  width = 9,
  height = 7
)

final_macro_plot <- make_time_plot(
  gold_macro_data,
  predictors,
  "Lagged Macro-Financial Predictors",
  split_date,
  "darkgreen"
)

show_and_save_plot(
  final_macro_plot,
  file.path(figures_dir, "final_macro_predictors_split.png"),
  width = 9,
  height = 9
)

seasonality_data <- train_data |>
  mutate(
    weekday = factor(
      weekdays(date, abbreviate = TRUE),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri")
    )
  )

final_weekday_plot <- ggplot(seasonality_data, aes(x = weekday, y = gold_log_return)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.2) +
  labs(
    title = "Gold Log Return by Weekday (Training Set)",
    x = NULL,
    y = "Gold log return"
  ) +
  theme_minimal()

show_and_save_plot(
  final_weekday_plot,
  file.path(figures_dir, "final_weekday_seasonality.png"),
  width = 8,
  height = 5
)

png(
  filename = file.path(figures_dir, "final_gold_acf_pacf.png"),
  width = 1200,
  height = 1200,
  res = 150
)
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
acf(train_data$gold_close, main = "ACF of Gold Close (Training Set)")
pacf(train_data$gold_close, main = "PACF of Gold Close (Training Set)")
acf(train_data$gold_log_return, main = "ACF of Gold Log Return (Training Set)")
pacf(train_data$gold_log_return, main = "PACF of Gold Log Return (Training Set)")
dev.off()

if (interactive()) {
  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))
  acf(train_data$gold_close, main = "ACF of Gold Close (Training Set)")
  pacf(train_data$gold_close, main = "PACF of Gold Close (Training Set)")
  acf(train_data$gold_log_return, main = "ACF of Gold Log Return (Training Set)")
  pacf(train_data$gold_log_return, main = "PACF of Gold Log Return (Training Set)")
}

baseline_all_metrics <- baseline_metrics |>
  mutate(
    model_class = "baseline",
    method = case_when(
      model == "mean" ~ "Mean benchmark",
      model == "naive" ~ "Naive benchmark"
    )
  )

ets_all_metrics <- ets_metrics |>
  mutate(model_class = "ets")

arima_all_metrics <- arima_metrics |>
  mutate(model_class = "arima")

arimax_all_metrics <- arimax_metrics |>
  mutate(model_class = "arimax")

all_model_metrics <- bind_rows(
  baseline_all_metrics,
  ets_all_metrics,
  arima_all_metrics,
  arimax_all_metrics
) |>
  mutate(
    display_label = model_label(model),
    model_class_label = model_class_label(model_class)
  )

holdout_ranking <- all_model_metrics |>
  filter(sample == "holdout") |>
  arrange(rmse) |>
  mutate(
    rank = row_number(),
    rmse_diff_from_naive = rmse - rmse[model == "naive"][1],
    coverage80_gap = coverage80 - 0.80,
    coverage95_gap = coverage95 - 0.95
  ) |>
  select(
    rank,
    model_class,
    model_class_label,
    model,
    display_label,
    method,
    rmse,
    rmse_diff_from_naive,
    coverage80,
    coverage80_gap,
    coverage95,
    coverage95_gap,
    avg_width80,
    avg_width95,
    aicc
  )

class_winners <- holdout_ranking |>
  group_by(model_class, model_class_label) |>
  slice_min(rmse, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(match(model_class, c("baseline", "ets", "arima", "arimax")))

winner_train_metrics <- all_model_metrics |>
  filter(sample == "train") |>
  select(model_class, model, rmse_train = rmse, aicc_train = aicc)

winner_holdout_metrics <- class_winners |>
  select(
    model_class,
    model,
    display_label,
    method,
    rmse_holdout = rmse,
    coverage80,
    coverage95,
    avg_width80,
    avg_width95,
    aicc_holdout = aicc
  )

winner_generalization_summary <- winner_holdout_metrics |>
  left_join(winner_train_metrics, by = c("model_class", "model")) |>
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

baseline_diagnostics <- baseline_residual_diagnostics |>
  mutate(
    model_class = "baseline",
    method = case_when(
      model == "mean" ~ "Mean benchmark",
      model == "naive" ~ "Naive benchmark"
    ),
    aicc = NA_real_
  ) |>
  select(model_class, model, method, aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue)

ets_diagnostics <- ets_residual_diagnostics |>
  mutate(model_class = "ets") |>
  select(model_class, model, method, aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue)

arima_diagnostics <- arima_residual_diagnostics |>
  mutate(model_class = "arima") |>
  select(model_class, model, method, aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue)

arimax_diagnostics <- arimax_residual_diagnostics |>
  mutate(model_class = "arimax") |>
  select(model_class, model, method, aicc, residual_mean, residual_sd, residual_acf1, ljung_box_pvalue)

winner_residual_diagnostics <- bind_rows(
  baseline_diagnostics,
  ets_diagnostics,
  arima_diagnostics,
  arimax_diagnostics
) |>
  inner_join(class_winners |> select(model_class, model, display_label), by = c("model_class", "model")) |>
  select(
    model_class,
    display_label,
    method,
    residual_mean,
    residual_sd,
    residual_acf1,
    ljung_box_pvalue
  )

selection_summary <- bind_rows(
  holdout_ranking |>
    slice(1) |>
    transmute(
      criterion = "Lowest holdout RMSE overall",
      winner = display_label,
      value = sprintf("%.3f", rmse)
    ),
  holdout_ranking |>
    filter(model_class != "baseline") |>
    slice(1) |>
    transmute(
      criterion = "Lowest holdout RMSE among non-baselines",
      winner = display_label,
      value = sprintf("%.3f", rmse)
    ),
  class_winners |>
    slice_max(coverage80, n = 1, with_ties = FALSE) |>
    transmute(
      criterion = "Best 80% interval coverage among finalists",
      winner = display_label,
      value = sprintf("%.3f", coverage80)
    ),
  class_winners |>
    slice_max(coverage95, n = 1, with_ties = FALSE) |>
    transmute(
      criterion = "Best 95% interval coverage among finalists",
      winner = display_label,
      value = sprintf("%.3f", coverage95)
    )
)

baseline_forecasts <- baseline_holdout_forecasts |>
  filter(model == "naive") |>
  transmute(
    date = as.Date(date),
    model_class = "baseline",
    model,
    actual,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )

ets_forecasts <- ets_holdout_forecasts |>
  transmute(
    date = as.Date(date),
    model_class = "ets",
    model,
    actual,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )

arima_forecasts <- arima_holdout_forecasts |>
  transmute(
    date = as.Date(date),
    model_class = "arima",
    model,
    actual,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )

arimax_forecasts <- arimax_holdout_forecasts |>
  transmute(
    date = as.Date(date),
    model_class = "arimax",
    model,
    actual,
    point_forecast,
    lo80,
    hi80,
    lo95,
    hi95
  )

shortlist_lookup <- class_winners |>
  select(model_class, model, display_label, model_class_label)

final_shortlist_forecasts <- bind_rows(
  baseline_forecasts,
  ets_forecasts,
  arima_forecasts,
  arimax_forecasts
) |>
  inner_join(shortlist_lookup, by = c("model_class", "model")) |>
  select(date, model_class, model_class_label, model, display_label, actual, point_forecast, lo80, hi80, lo95, hi95)

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
  round_report_table()

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
  round_report_table()

winner_generalization_report <- winner_generalization_summary |>
  mutate(model_class = model_class_label(model_class)) |>
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
  ) |>
  round_report_table()

winner_residual_diagnostics_report <- winner_residual_diagnostics |>
  mutate(
    model_class = model_class_label(model_class),
    residual_mean = round(residual_mean, 3),
    residual_sd = round(residual_sd, 3),
    residual_acf1 = round(residual_acf1, 3),
    ljung_box_pvalue = format_pvalue(ljung_box_pvalue)
  )

write_csv(
  holdout_ranking_report,
  file.path(processed_dir, "final_model_ranking_holdout.csv")
)
write_csv(
  class_winners_report,
  file.path(processed_dir, "final_class_winners.csv")
)
write_csv(
  winner_generalization_report,
  file.path(processed_dir, "final_winner_generalization_summary.csv")
)
write_csv(
  winner_residual_diagnostics_report,
  file.path(processed_dir, "final_winner_residual_diagnostics.csv")
)
write_csv(
  selection_summary,
  file.path(processed_dir, "final_selection_summary.csv")
)

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
  winner_residual_diagnostics_report,
  n = nrow(winner_residual_diagnostics_report)
)
print_section("Final selection summary", selection_summary, n = nrow(selection_summary))

class_palette <- c(
  Baseline = "#4C566A",
  ETS = "#1F78B4",
  ARIMA = "#33A02C",
  ARIMAX = "#E31A1C"
)

shortlist_palette <- c(
  "Naive" = "#4C566A",
  "ETS(M,N,N)" = "#1F78B4",
  "ARIMA(1,1,1) + drift" = "#33A02C",
  "ARIMAX market core (lag 1)" = "#E31A1C"
)

rmse_plot <- holdout_ranking |>
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
  scale_x_continuous(
    labels = label_number(accuracy = 0.001),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "Holdout RMSE Ranking Across All Models",
    subtitle = "Dashed line marks the naive benchmark",
    x = "Holdout RMSE",
    y = NULL,
    fill = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  rmse_plot,
  file.path(figures_dir, "final_holdout_rmse_ranking.png"),
  width = 10,
  height = 7
)

holdout_actual <- holdout_data |>
  transmute(date = as.Date(date), actual = gold_close)

overlay_plot <- ggplot() +
  geom_line(
    data = holdout_actual,
    aes(x = date, y = actual),
    color = "black",
    linewidth = 0.6
  ) +
  geom_line(
    data = final_shortlist_forecasts,
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
  theme_minimal()

show_and_save_plot(
  overlay_plot,
  file.path(figures_dir, "final_shortlist_holdout_overlay.png"),
  width = 10,
  height = 6
)

zoom_start <- holdout_actual |>
  slice_tail(n = 120) |>
  summarise(date = min(date)) |>
  pull(date)

zoom_plot <- ggplot() +
  geom_line(
    data = holdout_actual |> filter(date >= zoom_start),
    aes(x = date, y = actual),
    color = "black",
    linewidth = 0.7
  ) +
  geom_line(
    data = final_shortlist_forecasts |> filter(date >= zoom_start),
    aes(x = date, y = point_forecast, color = display_label),
    linewidth = 0.5
  ) +
  scale_color_manual(values = shortlist_palette) +
  labs(
    title = "Finalist Forecasts: Last 120 Holdout Observations",
    x = "Date",
    y = "Gold close",
    color = NULL
  ) +
  theme_minimal()

show_and_save_plot(
  zoom_plot,
  file.path(figures_dir, "final_shortlist_holdout_zoom.png"),
  width = 10,
  height = 6
)

coverage_plot_data <- class_winners |>
  select(display_label, coverage80, coverage95) |>
  pivot_longer(
    cols = c(coverage80, coverage95),
    names_to = "interval",
    values_to = "coverage"
  ) |>
  mutate(
    interval = recode(
      interval,
      coverage80 = "80% interval",
      coverage95 = "95% interval"
    ),
    nominal = if_else(interval == "80% interval", 0.80, 0.95)
  )

coverage_plot <- ggplot(
  coverage_plot_data,
  aes(x = fct_reorder(display_label, coverage), y = coverage, color = display_label)
) +
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
  theme_minimal()

show_and_save_plot(
  coverage_plot,
  file.path(figures_dir, "final_shortlist_interval_coverage.png"),
  width = 10,
  height = 6
)

if (cleanup_intermediate_files) {
  intermediate_csvs <- file.path(
    processed_dir,
    c(
      "baseline_metrics.csv",
      "baseline_residual_diagnostics.csv",
      "baseline_holdout_forecasts.csv",
      "eda_summary_stats.csv",
      "train_correlations.csv",
      "ets_metrics.csv",
      "ets_residual_diagnostics.csv",
      "ets_holdout_forecasts.csv",
      "ets_vs_baseline.csv",
      "arima_stationarity_summary.csv",
      "arima_candidate_models.csv",
      "arima_metrics.csv",
      "arima_residual_diagnostics.csv",
      "arima_holdout_forecasts.csv",
      "arima_vs_existing.csv",
      "arimax_feature_screening.csv",
      "arimax_candidate_models.csv",
      "arimax_coefficients.csv",
      "arimax_metrics.csv",
      "arimax_residual_diagnostics.csv",
      "arimax_holdout_forecasts.csv",
      "arimax_vs_existing.csv",
      "final_shortlist_forecasts.csv"
    )
  )

  unlink(intermediate_csvs[file.exists(intermediate_csvs)])
}

message(
  "\nSaved final variable summary to ", file.path(processed_dir, "final_variable_summary.csv"),
  "\nSaved final model ranking to ", file.path(processed_dir, "final_model_ranking_holdout.csv"),
  "\nSaved final class winners to ", file.path(processed_dir, "final_class_winners.csv"),
  "\nSaved winner generalization summary to ", file.path(processed_dir, "final_winner_generalization_summary.csv"),
  "\nSaved winner residual diagnostics to ", file.path(processed_dir, "final_winner_residual_diagnostics.csv"),
  "\nSaved final selection summary to ", file.path(processed_dir, "final_selection_summary.csv"),
  "\nSaved report-ready plots to ", figures_dir,
  "\nRemoved redundant intermediate CSVs: ", cleanup_intermediate_files
)
