#' @title kable_output
#'
#' @description IF not render then show the table and copy it to clipboard in latex format. Round & decimal mark also managed
#'
#' @return knitr::kable output \code{overview_print}
#' @examples
#' kable_output(iris)
#' @export
#'

kable_output <- function (.data, align = NULL, round_digits = NULL, plus_sign = FALSE,
                          hun = FALSE, same_digits = FALSE, verticals = FALSE, .keep_lines = c(1, 2, -1), force_large = FALSE, caption = NULL, ...)
{
  x <- ungroup(.data)
  numeric_cols <- select_if(x, is.numeric) %>% names()
  if (!is.null(round_digits)) {
    x <- mutate_at(x, vars(numeric_cols), round, digits = round_digits) %>%
      mutate_at(vars(numeric_cols), as.character)
  }
  if (plus_sign) {
    x <- mutate_all(x, as.character) %>% mutate_at(vars(numeric_cols),
                                                   ~if_else(str_starts(., "-"), ., str_c("+", .)))
  }
  if (hun) {
    x <- mutate_all(x, as.character) %>% mutate_at(vars(numeric_cols),
                                                   str_replace, "[.]", ",")
  }
  if (same_digits) {
    x <- mutate_all(x, as.character) %>% mutate_at(vars(numeric_cols),
                                                   function(values) {
                                                     out <- map_chr(values, ~str_c(., rep(0, times = max(str_length(values)) -
                                                                                            str_length(.))))
                                                     out
                                                   })
  }
  if (is.null(align)) {
    align <- c("l", rep("c", ncol(x) - 1))
  }
  tryCatch(expr = {

    latex_out <- knitr::kable(x, ..., format = "latex", align = align)
    if (!verticals) {
      latex_out <- latex_out %>%
        str_replace_all("l\\|", "l ") %>%
        str_replace_all("c\\|", "c ")
    }
    if (!is.null(.keep_lines)) {

      n_line <- str_count(latex_out, "hline")

      for (i in 1:n_line) {
        latex_out <- str_replace(latex_out, "hline", str_c("hlin", i))
      }

      .keep_lines <- ifelse(.keep_lines > 0, .keep_lines, n_line + 1 + .keep_lines)

      for (i in .keep_lines) {
        latex_out <- str_replace(latex_out, str_c("hlin", i), "hline")
      }

      for (i in n_line:1) {
        latex_out <- str_remove_all(latex_out, str_c("\\\\hlin", i))
      }

    }

    clipr::write_clip(latex_out)
    message(crayon::bgGreen("Table copied to clipboard in latex format!"))
  }, error = function(e) {
  })

  if (knitr::is_html_output()) {
    if (nrow(.data) > 10) {

      dt_out <- DT::datatable(x, selection = 'none', caption = caption)

      if (!is.null(round_digits)) {
        numeric_cols <- .data %>%
          map_lgl(is.numeric) %>%
          which()

        dt_out <- DT::formatRound(dt_out, numeric_cols, digits = round_digits)

      }

      dt_out
    } else {
      knitr::kable(x, ..., align = align, caption = caption)
    }
  } else if (knitr::is_latex_output()) {
    if (nrow(.data) < 30 | force_large) {
      knitr::kable(x, ..., align = align, caption = caption)
    }
  } else {
    .data
  }
}
