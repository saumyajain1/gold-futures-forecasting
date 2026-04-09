setup_project <- function(required_packages) {
  missing_packages <- required_packages[
    !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_packages) > 0) {
    stop(
      "Install these packages first: ",
      paste(missing_packages, collapse = ", "),
      call. = FALSE
    )
  }

  suppressPackageStartupMessages({
    invisible(lapply(required_packages, library, character.only = TRUE))
  })

  if (file.exists("gold-futures-forecasting.Rproj")) {
    return(".")
  }

  if (file.exists("../gold-futures-forecasting.Rproj")) {
    return("..")
  }

  stop("Run this script from the project root or the scripts folder.", call. = FALSE)
}

setup_analysis <- function(required_packages, create_raw_dir = FALSE) {
  project_root <- setup_project(required_packages)

  paths <- list(
    project_root = project_root,
    raw_dir = file.path(project_root, "data", "raw"),
    processed_dir = file.path(project_root, "data", "processed"),
    figures_dir = file.path(project_root, "figures")
  )

  dir.create(paths$processed_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$figures_dir, recursive = TRUE, showWarnings = FALSE)

  if (create_raw_dir) {
    dir.create(paths$raw_dir, recursive = TRUE, showWarnings = FALSE)
  }

  paths
}

read_processed <- function(processed_dir, file_name) {
  readr::read_csv(file.path(processed_dir, file_name), show_col_types = FALSE)
}

read_processed_files <- function(processed_dir, files) {
  lapply(files, read_processed, processed_dir = processed_dir)
}

load_split_data <- function(processed_dir) {
  list(
    train = read_processed(processed_dir, "train_data.csv"),
    holdout = read_processed(processed_dir, "holdout_data.csv")
  )
}

print_section <- function(title, object, n = NULL) {
  message("\n", title)

  if (is.null(n)) {
    print(object)
  } else {
    print(object, n = n)
  }
}

show_and_save_plot <- function(plot, path = NULL, width = 8, height = 6, dpi = 300) {
  if (interactive()) {
    print(plot)
  }

  if (!is.null(path)) {
    ggplot2::ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  }
}

save_base_plot <- function(path, width = 1200, height = 1200, res = 150, code) {
  png(filename = path, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  eval.parent(substitute(code))
}

show_base_plot <- function(code, error_message = NULL) {
  if (!interactive()) {
    return(invisible(NULL))
  }

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  tryCatch(
    eval.parent(substitute(code)),
    error = function(e) {
      if (!is.null(error_message)) {
        message(error_message)
      }
    }
  )
}

rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}

make_summary_table <- function(df, sample_name) {
  numeric_names <- setdiff(names(df), "date")

  dplyr::bind_rows(lapply(numeric_names, function(var_name) {
    values <- df[[var_name]]

    tibble::tibble(
      sample = sample_name,
      variable = var_name,
      n = length(values),
      mean = mean(values),
      sd = sd(values),
      min = min(values),
      q1 = unname(stats::quantile(values, 0.25)),
      median = stats::median(values),
      q3 = unname(stats::quantile(values, 0.75)),
      max = max(values)
    )
  }))
}

make_time_plot <- function(df, columns, title, split_date, color, ncol = 1) {
  df |>
    dplyr::select(date, dplyr::all_of(columns)) |>
    tidyr::pivot_longer(-date, names_to = "series", values_to = "value") |>
    ggplot2::ggplot(ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(color = color, linewidth = 0.4) +
    ggplot2::geom_vline(xintercept = split_date, linetype = "dashed", color = "firebrick") +
    ggplot2::facet_wrap(~series, ncol = ncol, scales = "free_y") +
    ggplot2::labs(title = title, x = "Date", y = NULL) +
    ggplot2::theme_minimal()
}

format_numeric_table <- function(df, columns = NULL, digits = 3) {
  if (is.null(columns)) {
    columns <- names(df)[vapply(df, is.numeric, logical(1))]
  }

  df |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(columns),
      ~ifelse(is.na(.x), NA_character_, sprintf(paste0("%.", digits, "f"), .x))
    ))
}

format_pvalue <- function(x) {
  ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
}

round_numeric_table <- function(df, digits = 3) {
  df |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ round(.x, digits)))
}

label_values <- function(values, mapping) {
  dplyr::coalesce(unname(mapping[values]), values)
}

model_label_map <- function() {
  c(
    mean = "Mean benchmark",
    naive = "Naive",
    ses = "SES",
    holt = "Holt",
    damped_holt = "Damped Holt",
    ets_auto = "ETS(M,N,N)",
    auto_selected = "ARIMA(2,1,2) + drift",
    arima_112 = "ARIMA(1,1,2) + drift",
    arima_211 = "ARIMA(2,1,1) + drift",
    arima_111 = "ARIMA(1,1,1) + drift",
    arimax_market_core_lag1 = "ARIMAX market core (lag 1)",
    arimax_screened_top2 = "ARIMAX screened top 2",
    arimax_screened_top3 = "ARIMAX screened top 3",
    arimax_screened_top4 = "ARIMAX screened top 4"
  )
}

model_class_label_map <- function() {
  c(
    baseline = "Baseline",
    ets = "ETS",
    arima = "ARIMA",
    arimax = "ARIMAX"
  )
}

add_model_labels <- function(df) {
  df <- df |>
    dplyr::mutate(display_label = label_values(model, model_label_map()))

  if ("model_class" %in% names(df)) {
    df <- df |>
      dplyr::mutate(model_class_label = label_values(model_class, model_class_label_map()))
  }

  df
}

summarise_residuals <- function(residuals, fitdf = 0) {
  residuals <- residuals[!is.na(residuals)]
  lb_lag <- max(1, min(10, floor(length(residuals) / 5)))

  tibble::tibble(
    residual_mean = mean(residuals),
    residual_sd = stats::sd(residuals),
    residual_acf1 = stats::acf(residuals, plot = FALSE)$acf[2],
    ljung_box_pvalue = stats::Box.test(
      residuals,
      lag = lb_lag,
      type = "Ljung-Box",
      fitdf = fitdf
    )$p.value
  )
}

summarise_forecasts <- function(forecasts, groups) {
  forecasts |>
    dplyr::mutate(
      covered80 = actual >= lo80 & actual <= hi80,
      covered95 = actual >= lo95 & actual <= hi95
    ) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(groups))) |>
    dplyr::summarise(
      rmse = rmse(actual, point_forecast),
      n_predictions = sum(!is.na(point_forecast)),
      coverage80 = mean(covered80, na.rm = TRUE),
      coverage95 = mean(covered95, na.rm = TRUE),
      avg_width80 = mean(hi80 - lo80, na.rm = TRUE),
      avg_width95 = mean(hi95 - lo95, na.rm = TRUE),
      .groups = "drop"
    )
}

make_holdout_plot <- function(train_data, holdout_data, forecasts, title, colors = NULL, train_tail = 120) {
  plot_data <- dplyr::bind_rows(
    utils::tail(train_data, train_tail) |>
      dplyr::transmute(date, series = "actual", value = gold_close),
    holdout_data |>
      dplyr::transmute(date, series = "actual", value = gold_close),
    forecasts |>
      dplyr::transmute(date, series = model, value = point_forecast)
  )

  plot <- ggplot2::ggplot(plot_data, ggplot2::aes(x = date, y = value, color = series)) +
    ggplot2::geom_line(linewidth = 0.5) +
    ggplot2::geom_vline(
      xintercept = max(train_data$date),
      linetype = "dashed",
      color = "grey40"
    ) +
    ggplot2::labs(title = title, x = "Date", y = "Gold close", color = NULL) +
    ggplot2::theme_minimal()

  if (!is.null(colors)) {
    plot <- plot + ggplot2::scale_color_manual(values = colors)
  }

  plot
}

make_interval_plot <- function(forecasts, title, subtitle = NULL) {
  plot <- ggplot2::ggplot(forecasts, ggplot2::aes(x = date, y = point_forecast)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo95, ymax = hi95), fill = "grey85") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = lo80, ymax = hi80), fill = "grey70") +
    ggplot2::geom_line(ggplot2::aes(y = actual), color = "black", linewidth = 0.4) +
    ggplot2::geom_line(color = "steelblue", linewidth = 0.4) +
    ggplot2::labs(title = title, subtitle = subtitle, x = "Date", y = "Gold close") +
    ggplot2::theme_minimal()

  if ("model" %in% names(forecasts) && dplyr::n_distinct(forecasts$model) > 1) {
    plot <- plot + ggplot2::facet_wrap(~model, ncol = 1, scales = "free_y")
  }

  plot
}
