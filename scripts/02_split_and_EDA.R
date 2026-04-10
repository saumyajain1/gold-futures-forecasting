source(if (file.exists("scripts/utils.R")) "scripts/utils.R" else "utils.R")

paths <- setup_analysis(c("readr", "dplyr", "tidyr", "ggplot2"))
processed_dir <- paths$processed_dir
figures_dir <- paths$figures_dir
predictors <- c(
  "usd_broad_index_lag1",
  "sp500_index_lag1",
  "wti_price_lag1",
  "treasury_10y_yield_lag1"
)

data <- read_processed(processed_dir, "gold_macro_data.csv")

split_index <- floor(0.70 * nrow(data))
split_date <- data$date[split_index]

train_data <- data[1:split_index, ]
holdout_data <- data[(split_index + 1):nrow(data), ]

write_csv(train_data, file.path(processed_dir, "train_data.csv"))
write_csv(holdout_data, file.path(processed_dir, "holdout_data.csv"))

split_summary <- tibble(
  dataset = c("full", "train", "holdout"),
  rows = c(nrow(data), nrow(train_data), nrow(holdout_data)),
  start_date = c(min(data$date), min(train_data$date), min(holdout_data$date)),
  end_date = c(max(data$date), max(train_data$date), max(holdout_data$date))
)

summary_stats <- bind_rows(
  make_summary_table(data, "full"),
  make_summary_table(train_data, "train"),
  make_summary_table(holdout_data, "holdout")
)

train_correlations <- train_data |>
  select(-date) |>
  cor()

yearly_summary <- data |>
  mutate(year = format(date, "%Y")) |>
  group_by(year) |>
  summarise(
    days = n(),
    mean_gold_close = mean(gold_close),
    sd_gold_close = sd(gold_close),
    min_gold_close = min(gold_close),
    max_gold_close = max(gold_close),
    mean_log_return = mean(gold_log_return),
    sd_log_return = sd(gold_log_return),
    .groups = "drop"
  )

seasonality_data <- train_data |>
  mutate(
    weekday = factor(
      weekdays(date, abbreviate = TRUE),
      levels = c("Mon", "Tue", "Wed", "Thu", "Fri")
    ),
    month = factor(format(date, "%b"), levels = month.abb)
  )

weekday_summary <- seasonality_data |>
  group_by(weekday) |>
  summarise(
    days = n(),
    mean_gold_close = mean(gold_close),
    mean_log_return = mean(gold_log_return),
    sd_log_return = sd(gold_log_return),
    .groups = "drop"
  )

month_summary <- seasonality_data |>
  group_by(month) |>
  summarise(
    days = n(),
    mean_gold_close = mean(gold_close),
    mean_log_return = mean(gold_log_return),
    sd_log_return = sd(gold_log_return),
    .groups = "drop"
  )

target_correlations <- tibble(
  predictor = predictors,
  corr_with_gold_close = train_correlations["gold_close", predictors],
  corr_with_gold_log_return = train_correlations["gold_log_return", predictors]
)

print_section("Split summary", split_summary)
print_section("Summary statistics", summary_stats, n = nrow(summary_stats))
print_section("Yearly gold summary", yearly_summary, n = nrow(yearly_summary))
print_section("Weekday seasonality summary", weekday_summary, n = nrow(weekday_summary))
print_section("Month-of-year seasonality summary", month_summary, n = nrow(month_summary))
print_section("Training-set correlation matrix", round(train_correlations, 3))
print_section("Target vs predictor correlations", target_correlations)

gold_plot <- make_time_plot(
  data,
  c("gold_close", "gold_log_return"),
  "Gold Series with Train-Holdout Split",
  split_date,
  "steelblue"
)

show_and_save_plot(
  gold_plot,
  file.path(figures_dir, "gold_series_split.png"),
  width = 9,
  height = 7
)

predictor_plot <- make_time_plot(
  data,
  predictors,
  "Lagged Macro-Financial Predictors",
  split_date,
  "darkgreen"
)

show_and_save_plot(
  predictor_plot,
  file.path(figures_dir, "macro_predictors_split.png"),
  width = 9,
  height = 9
)

weekday_plot <- ggplot(seasonality_data, aes(x = weekday, y = gold_log_return)) +
  geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.2) +
  labs(
    title = "Gold Log Return by Weekday (Training Set)",
    x = NULL,
    y = "Gold log return"
  ) +
  theme_minimal()

show_and_save_plot(
  weekday_plot,
  file.path(figures_dir, "weekday_seasonality.png"),
  width = 8,
  height = 5
)

correlation_plot_data <- as.data.frame(as.table(train_correlations))

correlation_plot <- ggplot(
  correlation_plot_data,
  aes(x = Var1, y = Var2, fill = Freq)
) +
  geom_tile() +
  geom_text(aes(label = round(Freq, 2)), size = 3) +
  scale_fill_gradient2(low = "firebrick", mid = "white", high = "darkgreen") +
  labs(
    title = "Training-Set Correlation Heatmap",
    x = NULL,
    y = NULL,
    fill = "Corr"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

show_and_save_plot(
  correlation_plot,
  file.path(figures_dir, "train_correlation_heatmap.png"),
  width = 8,
  height = 6
)

save_base_plot(
  file.path(figures_dir, "gold_acf_pacf_plots.png"),
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
  },
  "Skipping interactive ACF/PACF panel because the plot pane is too small. Resize the Plots pane and rerun if needed."
)

message(
  "Saved split files to ", processed_dir,
  "\nFull rows: ", nrow(data),
  "\nTraining rows: ", nrow(train_data),
  "\nHoldout rows: ", nrow(holdout_data),
  "\nTraining dates: ", min(train_data$date), " to ", max(train_data$date),
  "\nHoldout dates: ", min(holdout_data$date), " to ", max(holdout_data$date),
  "\nSplit date: ", split_date,
  "\nSaved main EDA figures to ", figures_dir
)
