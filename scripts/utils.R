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
    ggsave(path, plot = plot, width = width, height = height, dpi = dpi)
  }
}

rmse <- function(actual, forecast) {
  sqrt(mean((actual - forecast)^2, na.rm = TRUE))
}
