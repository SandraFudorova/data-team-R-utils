#' Merge "_other" Fields with Parent Columns
#'
#' For any columns ending in \code{"_other"} or \code{"_other2"}, this function searches for a matching
#' parent column (e.g., \code{"crop"} for \code{"crop_other"}) and combines them into the parent column.
#' Values are joined with a separator (default: \code{" | "}). The \code{"_other"} column is then removed.
#'
#' @param data A data frame containing the parent and "_other" columns.
#' @param sep A string used to separate merged values. Default is \code{" | "}.
#'
#' @return A data frame with merged columns. Issues a warning for unmatched "_other" fields.
#' @examples
#' Data <- merge_other(Data)                   # uses default " | "
#' Data <- merge_other(Data, sep = "; ")       # custom separator
#' @export
merge_other <- function(data, sep = " | ") {
  variables_other <- data %>%
    dplyr::select(dplyr::ends_with("_other"), dplyr::ends_with("_other2")) %>%
    names()
  
  for (other in variables_other) {
    parent <- gsub("_other$|_other2$", "", other)
    
    if (parent %in% names(data)) {
      new_col <- rlang::sym(parent)
      
      data <- data %>%
        tidyr::unite(
          !!new_col,
          dplyr::all_of(c(parent, other)),
          sep = sep,
          remove = FALSE,
          na.rm = TRUE
        ) %>%
        dplyr::select(-dplyr::all_of(other))
    } else {
      warning(sprintf("No matching parent column found for '%s'", other))
    }
  }
  
  return(data)
}
