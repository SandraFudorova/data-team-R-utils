#' Convert values using a lookup table
#'
#' Multiplies a numeric column by a conversion factor based on a unit column,
#' using a named vector lookup (e.g. land, weight, volume, currency).
#'
#' @param df A data frame.
#' @param unit_col Name of the column containing unit labels (string).
#' @param value_col Name of the column containing numeric values (string).
#' @param output_col Name of the output column (string or symbol).
#' @param conversions Named numeric vector mapping units -> conversion factor.
#'
#' @return `df` with an added or updated `output_col`.
#' @export
#'
#' @examples
#' df <- tibble::tibble(unit = c("kg", "g"), value = c(2, 500))
#' conv <- c(kg = 1, g = 0.001)
#' convert(df, "unit", "value", value_kg, conv)
convert <- function(df, unit_col, value_col, output_col, conversions) {
  df %>%
    dplyr::mutate(
      {{ output_col }} :=
        .data[[value_col]] * conversions[.data[[unit_col]]]
    )
}

