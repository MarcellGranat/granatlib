#' @title Display message.
#'
#' @description Print a nicely formatted message, optionally with system time.
#' @param msg The message to display. Use {} for glue type interpolating. It can also detect the x in map using the get() function, but putting it into a function would cause an error, and simple bold text will be retrived.
#' @param type Character. One of "info" (default)/"ok"/"warning". This will determine the color and the symbol.
#' @param add_time Logical. Should the time printed? (default TRUE)
#'
#' @examples
#' x = 3
#' info("The value of x is {x}.", "info", add_time = TRUE)
#'
#' purrr::walk(1:10, \(x) info("{x} is bold!", "ok"))
#'
#' purrr::walk(1:10, \(x) info("{x + 1} is bold!", "warning"))
#' @export
#'

info <- function(msg = "", type = "info", add_time = TRUE) {

  tryCatch({
    if (stringr::str_detect(msg, "[{]") & stringr::str_detect(msg, "[}]")) {
      bold_text <- tryCatch({
        stringr::str_extract(msg, "[{].*?[}]") |>
          stringr::str_glue()
      }, error = \(e) NA_character_)

      if (is.na(bold_text)) {
        bold_text <- tryCatch({
          stringr::str_extract(msg, "[{].*?[}]") |>
            stringr::str_remove_all("[{]|[}]") |>
            get(envir = parent.frame())
        }, error = \(e) stringr::str_remove_all(stringr::str_extract(msg, "[{].*?[}]"), "[{]|[}]"))

      }

      bold_text <- crayon::bold(bold_text) |>
        (\(b_text) {
          dplyr::case_when(
            type == "info" ~ crayon::blue(b_text),
            type == "ok" ~ crayon::green(b_text),
            type == "warning" ~ crayon::red(b_text),
            TRUE ~ b_text
          )
        }) ()

      msg <- stringr::str_replace(msg, "[{].*?[}]", bold_text)
    }

    if (!type %in% c("info", "ok", "warning")) {
      type <- "info"
    }

    msg <- dplyr::case_when(
      type == "info" ~ paste(crayon::blue(clisymbols::symbol$info), msg),
      type == "ok" ~ paste(crayon::green(clisymbols::symbol$tick), msg),
      type == "warning" ~ paste(crayon::red(clisymbols::symbol$warning), msg),
      TRUE ~ msg
    )

    if (add_time) {
      msg <- paste(msg, crayon::magenta(paste0("(", format(Sys.time(), "%H:%M"), ")")))
    }

    message(msg)
  }, error = \(e) message("Error in info."))
}
