if (file.exists("scripts/utils.R")) {
  source("scripts/utils.R")
} else {
  source("utils.R")
}

project_root <- setup_project(c("readr", "dplyr", "tidyr", "ggplot2"))

processed_dir <- file.path(project_root, "data", "processed")
figures_dir <- file.path(project_root, "figures")
predictors <- c(
  "usd_broad_index_lag1",
  "sp500_index_lag1",
  "wti_price_lag1",
  "treasury_10y_yield_lag1"
)

dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

data <- read_csv(
  file.path(processed_dir, "gold_macro_data.csv"),
  show_col_types = FALSE
)

split_index <- floor(0.8 * nrow(data))
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

make_time_plot <- function(df, columns, title, color, ncol = 1) {
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
  "darkgreen"
)

show_and_save_plot(
  predictor_plot,
  file.path(figures_dir, "macro_predictors_split.png"),
  width = 9,
  height = 9
)

distribution_plot_data <- bind_rows(
  train_data |> mutate(sample = "train"),
  holdout_data |> mutate(sample = "holdout")
) |>
  select(sample, gold_close, gold_log_return) |>
  pivot_longer(-sample, names_to = "series", values_to = "value")

distribution_plot <- ggplot(distribution_plot_data, aes(x = value, fill = sample)) +
  geom_histogram(bins = 40, alpha = 0.6, position = "identity") +
  facet_wrap(~series, scales = "free", ncol = 1) +
  labs(
    title = "Distributions of Gold Price and Gold Log Return",
    x = NULL,
    y = "Count"
  ) +
  theme_minimal()

show_and_save_plot(distribution_plot)

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

month_plot <- ggplot(seasonality_data, aes(x = month, y = gold_log_return)) +
  geom_boxplot(fill = "darkgreen", alpha = 0.7, outlier.alpha = 0.2) +
  labs(
    title = "Gold Log Return by Month (Training Set)",
    x = NULL,
    y = "Gold log return"
  ) +
  theme_minimal()

show_and_save_plot(month_plot)

scatter_plot_data <- train_data |>
  select(gold_log_return, all_of(predictors)) |>
  pivot_longer(-gold_log_return, names_to = "predictor", values_to = "value")

scatter_plot <- ggplot(scatter_plot_data, aes(x = value, y = gold_log_return)) +
  geom_point(alpha = 0.25, size = 0.7, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick", linewidth = 0.5) +
  facet_wrap(~predictor, scales = "free_x") +
  labs(
    title = "Gold Log Return vs Lagged Predictors (Training Set)",
    x = NULL,
    y = "Gold log return"
  ) +
  theme_minimal()

show_and_save_plot(scatter_plot)

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

png(
  filename = file.path(figures_dir, "gold_acf_pacf_plots.png"),
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
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  tryCatch(
    {
      par(mfrow = c(2, 2), mar = c(3, 3, 2, 1))
      acf(train_data$gold_close, main = "ACF of Gold Close (Training Set)")
      pacf(train_data$gold_close, main = "PACF of Gold Close (Training Set)")
      acf(train_data$gold_log_return, main = "ACF of Gold Log Return (Training Set)")
      pacf(train_data$gold_log_return, main = "PACF of Gold Log Return (Training Set)")
    },
    error = function(e) {
      message(
        "Skipping interactive ACF/PACF panel because the plot pane is too small. ",
        "Resize the Plots pane and rerun if you want to see it."
      )
    }
  )
}

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
