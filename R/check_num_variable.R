#' Check Numeric Variable Summary and Distribution
#'
#' Prints summary statistics, missing value info, and optionally a histogram for a numeric variable.
#' Automatically calculates histogram bins using the Freedman–Diaconis rule unless specified manually.
#'
#' @param data A data frame containing the variable.
#' @param var An unquoted column name of a numeric variable.
#' @param show_plot Logical. Whether to show a histogram. Default is \code{TRUE}.
#' @param bins Optional. Number of bins to use in the histogram. If \code{NULL}, it is calculated automatically.
#'
#' @return Invisibly returns \code{NULL} after printing output and plotting (if applicable).
#'
#' @examples
#' check_num_variable(Data, f_focus_quant_prod, bins = 50)
#'
#' @export
check_num_variable <- function(data, var, show_plot = TRUE, bins = NULL) {
  var_sym <- rlang::ensym(var)
  var_name <- rlang::as_name(var_sym)
  x <- data[[var_name]]

  if (!is.numeric(x)) {
    stop(paste(var_name, "is not numeric."))
  }

  if (all(is.na(x))) {
    cat("All values in", var_name, "are NA.\n")
    return(invisible(NULL))
  }

  cat("Variable:", var_name, "\n\n")

  cat("📊 Summary Statistics:\n")
  print(summary(x))

  cat("\n❓ Missing values:", sum(is.na(x)), "\n")
  cat("🔢 Unique values:", length(unique(x)), "\n")
  cat("🟡 Percentage of zero values:", round(mean(x == 0, na.rm = TRUE) * 100, 2), "%\n\n")

  if (show_plot) {
    # Determine bins automatically if not provided
    if (is.null(bins)) {
      iqr <- IQR(x, na.rm = TRUE)
      n <- sum(!is.na(x))
      bin_width <- 2 * iqr / (n^(1/3))
      bins <- if (bin_width > 0) ceiling((max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / bin_width) else 30
    }

    print(
      ggplot(data, aes(x = !!var_sym)) +
        geom_histogram(bins = bins, fill = "skyblue", color = "black") +
        labs(title = paste("Histogram of", var_name), x = var_name, y = "Count") +
        theme_minimal()
    )
  }
}
