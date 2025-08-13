#' Clean Raw Survey Data
#'
#' Applies a standard cleaning pipeline to survey data by:
#' - Converting dates (if present)
#' - Removing specified irrelevant columns
#' - Lowercasing column names
#' - Keeping only farmers that "accepted to participate"
#' - Dropping fully empty columns
#' - Optionally deduplicating rows
#' - Logging cleaning steps
#'
#' @param data A data frame to clean.
#' @param irrelevant_columns A character vector of column names to remove.
#' @param deduplicate_by Optional. Name of the column to deduplicate by (e.g., \code{"uuid"}). Default is \code{NULL}.
#' @param log_file A file path to write log messages to. Default is \code{"cleaning_log.txt"}.
#'
#' @return A cleaned data frame.
#' @examples
#' Data <- clean_raw_data(Data, irrelevant_columns = "farmer_sample", deduplicate_by =
#' "farmer-code")
#'
#' @export
clean_raw_data <- function(data, irrelevant_columns = NULL, deduplicate_by = NULL,
                           log_file = "cleaning_log.txt") {
  cleaned <- data
  logs <- c("CLEANING SUMMARY", "---------------------")

  # Convert submissiondate to Date format if present
  if ("submissiondate" %in% colnames(cleaned)) {
    cleaned <- cleaned %>%
      mutate(submissiondate = as.Date(submissiondate, format = "%d-%m-%Y"))
  }

  # Remove irrelevant columns if specified
  if (!is.null(irrelevant_columns)) {
    cleaned <- cleaned %>% select(-all_of(irrelevant_columns))
  }

  # Lowercase column names
  cleaned <- cleaned %>% rename_with(tolower)

  # Filter "accepted to participate"
  match_col <- cleaned %>%
    summarise(across(everything(), ~ any(grepl("accepted to participate", .x, ignore.case = TRUE)))) %>%
    pivot_longer(everything(), names_to = "col", values_to = "match") %>%
    filter(match) %>%
    pull(col)

  if (length(match_col) == 1) {
    n_before <- nrow(cleaned)
    cleaned <- cleaned %>%
      filter(tolower(.data[[match_col]]) == "accepted to participate")
    n_after <- nrow(cleaned)
    n_removed <- n_before - n_after

    logs <- c(logs, paste("Farmers that refused to participate:", n_removed))
  } else if (length(match_col) > 1) {
    warning("Multiple columns matched 'accepted to participate'. No filtering applied.")
  } else {
    warning("No column matched 'accepted to participate'. No filtering applied.")
  }

  # Remove fully empty columns
  empty_cols <- names(cleaned)[sapply(cleaned, function(col) all(is.na(col)))]
  if (length(empty_cols) > 0) {
    cleaned <- cleaned %>% select(where(~ !all(is.na(.))))
    logs <- c(logs, paste0("Empty columns removed: ", paste(empty_cols, collapse = ", ")))
  }

  # Deduplicate
  if (is.null(deduplicate_by)) {
    logs <- c(logs, "Deduplication not applied: 'deduplicate_by' is NULL.")
  } else if (!(deduplicate_by %in% colnames(cleaned))) {
    logs <- c(logs, paste0("Deduplication not applied: column '", deduplicate_by, "' not found."))
  } else {
    duplicated_rows <- cleaned %>% filter(duplicated(.data[[deduplicate_by]]))
    logs <- c(logs, paste0("Deduplicated by '", deduplicate_by, "'."))

    logs <- c(logs, paste0("Duplicate rows removed: ", nrow(duplicated_rows)))
    if (nrow(duplicated_rows) > 0) {
      logs <- c(
        logs,
        paste0("IDs removed: ", paste(unique(duplicated_rows[[deduplicate_by]]), collapse = ", "))
      )
      cleaned <- cleaned %>% distinct(across(all_of(deduplicate_by)), .keep_all = TRUE)
    }
  }

  logs <- c(logs, "---------------------")

  # Print
  message("\n", paste(logs, collapse = "\n"))

  return(cleaned)
}
