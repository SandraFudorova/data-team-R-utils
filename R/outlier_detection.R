#' Outlier Detection Across All Numeric Non-Binary Variables (with Summary)
#'
#' Detects outliers in all numeric, non-binary columns using the mean ± 3*SD rule
#' and replaces them with a specified value. Prints a summary of how many values were replaced.
#'
#' @param data A data frame.
#' @param replacement The value to replace outliers with. Default is 9997.
#' @param exclude_columns Optional character vector of column names to exclude from outlier detection.
#'
#' @return A modified data frame with outliers replaced.
#' @examples
#' Data <- outlier_detection(Data, replacement = 9997, exclude_columns = c("mobile_number_farmer", "pi_geolocation"))
#'
#' @export
outlier_detection <- function(data, replacement = 9997, exclude_columns = NULL) {
  for (col in names(data)) {
    if (!is.null(exclude_columns) && col %in% exclude_columns) {
      next
    }

    x <- data[[col]]

    # Check: is numeric and not binary
    if (is.numeric(x)) {
      unique_vals <- unique(x[!is.na(x)])

      if (length(unique_vals) > 2 || !all(unique_vals %in% c(0, 1))) {
        mean_val <- mean(x, na.rm = TRUE)
        sd_val <- sd(x, na.rm = TRUE)
        is_outlier <- x > (mean_val + 3 * sd_val) | x < (mean_val - 3 * sd_val)

        n_replaced <- sum(is_outlier, na.rm = TRUE)
        total <- sum(!is.na(x))
        pct <- round(n_replaced / total * 100, 2)

        if (n_replaced > 0) {
          message(sprintf("'%s': Replaced %d values (%.2f%%)", col, n_replaced, pct))
        }

        data[[col]] <- ifelse(is_outlier, replacement, x)
      }
    }
  }

  return(data)
}
