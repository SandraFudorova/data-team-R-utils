#' Check Categorical Variable Frequencies and Plot
#'
#' Prints a frequency table and missing value count for a categorical variable (character or factor),
#' and optionally shows a bar plot of the top 10 categories. If more than 10 categories exist,
#' less frequent ones are grouped into "Other".
#'
#' @param data A data frame containing the variable.
#' @param var An unquoted column name of a character or factor variable.
#' @param show_plot Logical. Whether to show a bar plot. Default is \code{TRUE}.
#'
#' @return Invisibly returns \code{NULL} after printing output and plotting (if applicable).
#' @examples
#' check_cat_variable(Data, f_coffee_farm_age, show_plot = FALSE)
#'
#' @export
check_cat_variable <- function(data, var, show_plot = TRUE) {
  var_sym <- rlang::ensym(var)
  var_name <- rlang::as_name(var_sym)
  x <- data[[var_name]]

  if (!is.character(x) && !is.factor(x)) {
    stop(paste(var_name, "is not a character or factor variable."))
  }

  cat("🔎 Variable:", var_name, "\n\n")

  # Frequency table
  freq_table <- table(x, useNA = "ifany") %>% sort(decreasing = TRUE)
  print(freq_table)

  cat("\n❓ Missing values:", sum(is.na(x)), "\n")
  cat("🔢 Unique categories:", length(unique(x)), "\n")

  if (show_plot) {
    print(
      data %>%
        filter(!is.na(!!var_sym)) %>%
        mutate(!!var_sym := forcats::fct_lump_n(!!var_sym, n = 10, other_level = "Other")) %>%
        ggplot(aes(x = !!var_sym)) +
        geom_bar(fill = "steelblue") +
        labs(title = paste("Top 10 Categories in", var_name),
             x = var_name, y = "Count") +
        theme_minimal() +
        coord_flip()
    )
  }
}
