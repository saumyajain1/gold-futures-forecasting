if (file.exists("scripts/utils.R")) {
  source("scripts/utils.R")
} else {
  source("utils.R")
}

project_root <- setup_project(c("quantmod", "dplyr", "readr", "tidyr"))

raw_dir <- file.path(project_root, "data", "raw")
processed_dir <- file.path(project_root, "data", "processed")

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

download_fred <- function(series_id, file_name, column_name) {
  url <- paste0("https://fred.stlouisfed.org/graph/fredgraph.csv?id=", series_id)
  path <- file.path(raw_dir, file_name)

  download.file(url, path, method = "curl", quiet = TRUE, extra = "--http1.1")

  read_csv(path, show_col_types = FALSE, na = c(".", "")) |>
    rename(date = observation_date, !!column_name := all_of(series_id)) |>
    mutate(date = as.Date(date))
}

message("Downloading gold futures data.")
gold_xts <- getSymbols("GC=F", src = "yahoo", auto.assign = FALSE, warnings = FALSE)

gold_data <- tibble(
  date = as.Date(index(gold_xts)),
  gold_close = as.numeric(gold_xts[, "GC=F.Close"])
) |>
  arrange(date) |>
  filter(!is.na(gold_close))

write_csv(gold_data, file.path(raw_dir, "gold_futures_gc_f_yahoo.csv"))

message("Downloading macro-financial predictors.")
usd_data <- download_fred("DTWEXBGS", "usd_broad_index_fred.csv", "usd_broad_index")
sp500_data <- download_fred("SP500", "sp500_index_fred.csv", "sp500_index")
wti_data <- download_fred("DCOILWTICO", "wti_price_fred.csv", "wti_price")
treasury_data <- download_fred("DGS10", "treasury_10y_yield_fred.csv", "treasury_10y_yield")

message("Merging and lagging predictors.")
gold_macro_data <- gold_data |>
  left_join(usd_data, by = "date") |>
  left_join(sp500_data, by = "date") |>
  left_join(wti_data, by = "date") |>
  left_join(treasury_data, by = "date") |>
  arrange(date) |>
  fill(usd_broad_index, sp500_index, wti_price, treasury_10y_yield, .direction = "down") |>
  mutate(
    gold_log_return = log(gold_close) - log(lag(gold_close)),
    usd_broad_index_lag1 = lag(usd_broad_index),
    sp500_index_lag1 = lag(sp500_index),
    wti_price_lag1 = lag(wti_price),
    treasury_10y_yield_lag1 = lag(treasury_10y_yield)
  ) |>
  select(
    date,
    gold_close,
    gold_log_return,
    usd_broad_index_lag1,
    sp500_index_lag1,
    wti_price_lag1,
    treasury_10y_yield_lag1
  )

gold_macro_data <- gold_macro_data[complete.cases(gold_macro_data), ]

write_csv(gold_macro_data, file.path(processed_dir, "gold_macro_data.csv"))

message(
  "Saved processed data to ",
  file.path(processed_dir, "gold_macro_data.csv"),
  "\nRows: ", nrow(gold_macro_data),
  "\nDate range: ",
  min(gold_macro_data$date),
  " to ",
  max(gold_macro_data$date)
)
