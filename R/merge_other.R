#' Merge "_other" Fields with Parent Columns
#'
#' For any columns ending in \code{"_other"} or \code{"_other2"}, this function searches for a matching
#' parent column (e.g., \code{"crop"} for \code{"crop_other"}) and combines them into the parent column.
#' Values are joined with a separator (default: \code{" | "}). The \code{"_other"} column is then removed.
#'
#' @param data A data frame containing the parent and "_other" columns.
#'
#' @return A data frame with merged columns. Issues a warning for unmatched "_other" fields.
#' @examples
#' Data <- merge_other(Data)
#' @export
merge_other <- function(data) {
  variables_other <- data %>%
    select(ends_with("_other"), ends_with("_other2")) %>%
    names()

  for (other in variables_other) {
    parent <- gsub("_other$|_other2$", "", other)

    if (parent %in% names(data)) {
      new_col <- sym(parent)

      data <- data %>%
        unite(
          !!new_col,
          all_of(c(parent, other)),
          sep = " | ",
          remove = FALSE,
          na.rm = TRUE
        ) %>%
        select(-all_of(other))
    } else {
      warning(sprintf("No matching parent column found for '%s'", other))
    }
  }

  return(data)
}
