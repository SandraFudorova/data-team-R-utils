#' Replace Special Placeholder Values with NA
#'
#' Replaces numeric placeholders like 9999, 9998, 9997 and text placeholders like
#' "I don't know" or "I prefer not to say" with NA across all applicable columns.
#'
#' @param data A data frame.
#' @param numeric_values A vector of numeric values to replace with NA. Default: c(9999, 9998, 9997)
#' @param character_values A vector of character strings to replace with NA (case-insensitive).
#'                         Default: common survey placeholders.
#'
#' @return A cleaned data frame with placeholder values replaced.
#' @examples
#' Data <- replace_placeholders(Data,character_values = c("i don't know", "i prefer not to say", "9999", "9998", "nill"))
#'
#' @export
replace_placeholders <- function(data,
                                 numeric_values = c(9999, 9998, 9997),
                                 character_values = c("i don't know", "i prefer not to say", "9999", "9998")) {

  total_replacements <- 0

  # Replace numeric placeholders
  for (val in numeric_values) {
    before <- sum(data[sapply(data, is.numeric)] == val, na.rm = TRUE)
    data <- data %>%
      mutate(across(where(is.numeric), ~na_if(., val)))
    total_replacements <- total_replacements + before
  }

  # Replace character placeholders (case-insensitive match, but retain original case)
  char_cols <- which(sapply(data, is.character))

  for (col_index in char_cols) {
    col_name <- names(data)[col_index]
    original_col <- data[[col_name]]
    lower_col <- tolower(original_col)

    is_match <- lower_col %in% tolower(character_values)
    total_replacements <- total_replacements + sum(is_match, na.rm = TRUE)

    data[[col_name]][is_match] <- NA
  }

  message(sprintf("\n \n Replaced %d placeholder values with NA", total_replacements))
  return(data)
}
