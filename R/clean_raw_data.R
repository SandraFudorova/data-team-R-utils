#' Clean Raw Survey Data
#'
#' Applies a standard cleaning pipeline to survey data by:
#' - Converting dates (if present)
#' - Removing specified irrelevant columns
#' - Removing trailing spaces in character columns
#' - Lowercasing column names
#' - Keeping only farmers that "accepted to participate"
#' - Dropping fully empty columns
#' - Optionally deduplicating rows
#'
#' @param data A data frame to clean.
#' @param irrelevant_columns A character vector of column names to remove.
#' @param deduplicate_by Optional. Name of the column to deduplicate by (e.g., \code{"uuid"}). Default is \code{NULL}.
#'
#' @return A cleaned data frame.
#' @examples
#' Data <- clean_raw_data(Data, irrelevant_columns = "farmer_sample", deduplicate_by =
#' "farmer-code")
#'
#' @export
clean_raw_data <- function(data, irrelevant_columns = NULL, deduplicate_by = NULL) {
  cleaned <- data
  logs <- c("CLEANING SUMMARY", "---------------------")

  # Remove irrelevant columns if specified
  if (!is.null(irrelevant_columns)) {
    cleaned <- cleaned %>% select(-all_of(irrelevant_columns))
    logs <- c(logs, "Irrelevant columns removed.")
  } else {
    logs <- c(logs, "Irrelevant column removal skipped.")
  }

  # Deduplicate
  if (is.null(deduplicate_by)) {
    logs <- c(logs, "Deduplication not applied: 'deduplicate_by' is NULL.")
  } else if (!(deduplicate_by %in% colnames(cleaned))) {
    logs <- c(logs, paste0("Deduplication not applied: column '", deduplicate_by, "' not found."))
  } else {
    duplicated_rows <- cleaned %>% filter(duplicated(.data[[deduplicate_by]]))
    cleaned <- cleaned %>% distinct(across(all_of(deduplicate_by)), .keep_all = TRUE)
    logs <- c(logs, "Deduplication applied.")
  }

  # Lowercase column names
  cleaned <- cleaned %>% rename_with(tolower)
  logs <- c(logs, "Column names converted to lowercase.")

  # Remove trailing spaces in character columns
  cleaned <- cleaned %>% mutate_if(is.character, str_trim)
  logs <- c(logs, "Trailing spaces removed from character columns.")

  # Convert submissiondate or submission date to Date format if present
  date_col <- intersect(c("submissiondate", "submission date"), colnames(cleaned))
  if (length(date_col) == 1) {
    cleaned <- cleaned %>%
      mutate(!!sym(date_col) := as.Date(.data[[date_col]], format = "%d-%m-%Y"))
    logs <- c(logs, paste0("Date conversion applied to column '", date_col, "'."))
  } else {
    logs <- c(logs, "Date conversion not applied.")
  }

  # Filter "accepted to participate"
  match_col <- cleaned %>%
    summarise(across(everything(), ~ any(grepl("accepted to participate", .x, ignore.case = TRUE)))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "match") %>%
    filter(match) %>%
    pull(col)

  if (length(match_col) == 1) {
    cleaned <- cleaned %>%
      filter(tolower(.data[[match_col]]) == "accepted to participate")
    logs <- c(logs, "Consent verified: only farmers that 'accepted to participate' kept.")
  } else if (length(match_col) > 1) {
    logs <- c(logs, "Consent not verified: multiple columns contain 'accepted to participate'.")
  } else {
    logs <- c(logs, "Consent not verified: no column contains 'accepted to participate'.")
  }

  # Remove fully empty columns
  empty_cols <- names(cleaned)[sapply(cleaned, function(col) all(is.na(col)))]
  if (length(empty_cols) > 0) {
    cleaned <- cleaned %>% select(where(~ !all(is.na(.))))
    logs <- c(
      logs,
      sprintf(
        "Empty columns removed: %s",
        paste(empty_cols, collapse = ", ")
      )
    )
  } else {
    logs <- c(logs, "No empty columns to remove.")
  }

  logs <- c(logs, "---------------------")

  # Print logs
  message("\n", paste(logs, collapse = "\n"))

  return(cleaned)
}
