#' @title kable_output
#'
#' @description IF not render then show the table and copy it to clipboard in latex format. Round & decimal mark also managed
#'
#' @return knitr::kable output \code{overview_print}
#' @examples
#' kable_output(iris)
#' @export
#'


kable_output <- function(.data, align = NULL, round_digits = NULL, plus_sign = FALSE, hun = FALSE, same_digits = FALSE, ...) {
  x <- ungroup(.data)

  numeric_cols <- select_if(x, is.numeric) %>%
    names()

  if (!is.null(round_digits)) {
    x <- mutate_at(x, vars(numeric_cols), round, digits = round_digits) %>%
      mutate_at(vars(numeric_cols), as.character)
  }

  if (plus_sign) {
    x <- mutate_all(x, as.character) %>%
      mutate_at(vars(numeric_cols), ~ if_else(str_starts(., "-"), ., str_c("+", .)))
  }

  if (hun) {
    x <- mutate_all(x, as.character) %>%
      mutate_at(vars(numeric_cols), str_replace, "[.]", ",")
  }

  if (same_digits) {
    x <- mutate_all(x, as.character) %>%
      mutate_at(vars(numeric_cols), function(values) {
        out <- map_chr(values, ~ str_c(., rep(0, times = max(str_length(values)) - str_length(.))))
        out
      }
      )
  }

  if (is.null(align)) {
    align <- c("l", rep("c", ncol(x) - 1))
  }

  tryCatch(
    expr = {
      knitr::kable(x, ..., format = "latex", align = align) %>%
        clipr::write_clip()

      message(crayon::bgGreen("Table copied to clipboard in latex format!"))
    },
    error = function(e) {}
  )
  knitr::kable(x, ..., align = align)
}
