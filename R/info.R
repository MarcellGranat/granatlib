#' @title Display message.
#'
#' @description Print a nicely formatted message, optionally with system time.
#' @param msg The message to display. Use {} for glue type interpolating.
#' @param type Character. One of "info" (default)/"ok"/"warning". This will determine the color and the symbol.
#' @param add_time Logical. Shoudl the time printed? (default TRUE)
#'
#' @examples
#' x = 3
#' info("The value of x is {x}.", "info", add_time = TRUE)
#'
#' @export
#'

info <- function(msg = "", type = "info", add_time = TRUE) {

  if (stringr::str_detect(msg, "[{]") & stringr::str_detect(msg, "[}]")) {
    bold_text <- stringr::str_extract(msg, "[{].*?[}]") |>
      stringr::str_glue() |>
      crayon::bold() |>
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

  msg <- case_when(
    type == "info" ~ paste(crayon::blue(clisymbols::symbol$info), msg),
    type == "ok" ~ paste(crayon::green(clisymbols::symbol$tick), msg),
    type == "warning" ~ paste(crayon::red(clisymbols::symbol$warning), msg),
    TRUE ~ msg
  )

  if (add_time) {
    msg <- paste(msg, crayon::magenta(paste0("(", format(Sys.time(), "%H:%M"), ")")))
  }

  message(msg)

}
