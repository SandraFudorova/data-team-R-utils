#' Correct Zero Quantities for Production, Sales, Loss, and optionally Premium Sales
#'
#' Replaces 0 values in quantity-related columns with NA when both production and sales are 0.
#'
#' @param data A data frame with columns: `f_focus_quant_prod`, `f_focus_quant_sold`,
#'        `f_focus_quant_lost`, and optionally `f_focus_quant_sold_premium`.
#' @param include_premium Logical. Whether to include `f_focus_quant_sold_premium` in the correction. Default is TRUE.
#'
#' @return A modified data frame with corrections applied and messages summarizing changes.
#' @examples
#' Data <- correct_quantities(Data, include_premium = FALSE)
#'
#' @export

correct_quantities <- function(data, include_premium = TRUE) {
  # condition: both prod and sold are zero
  both_zero <- data$f_focus_quant_prod == 0 & data$f_focus_quant_sold == 0

  # count changes before replacing
  message(sum(both_zero & !is.na(data$f_focus_quant_prod)),  " changed in f_focus_quant_prod")
  message(sum(both_zero & !is.na(data$f_focus_quant_sold)),  " changed in f_focus_quant_sold")
  message(sum(both_zero & !is.na(data$f_focus_quant_lost)),  " changed in f_focus_quant_lost")

  # replacements
  data <- data %>%
    mutate(
      f_focus_quant_prod = ifelse(both_zero, NA, f_focus_quant_prod),
      f_focus_quant_sold = ifelse(both_zero, NA, f_focus_quant_sold),
      f_focus_quant_lost = ifelse(both_zero, NA, f_focus_quant_lost)
    )

  # premium handling
  if (include_premium && "f_focus_quant_sold_premium" %in% names(data)) {
    message(sum(both_zero & !is.na(data$f_focus_quant_sold_premium)),
            " changed in f_focus_quant_sold_premium")
    data <- data %>%
      mutate(f_focus_quant_sold_premium = ifelse(both_zero, NA, f_focus_quant_sold_premium))
  }

  data
}
