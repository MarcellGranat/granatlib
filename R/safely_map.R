safely_map <- function (.x, .f, ..., .id = NULL, msg = NULL, quiet = TRUE, otherwise = NULL, .keep_null = TRUE) {
  if (is.null(msg)) {
    msg <- deparse(substitute(.f))
  }
  message(crayon::magenta(msg))
  .f <- purrr::as_mapper(purrr::safely(.f, quiet = TRUE, otherwise = otherwise), ...)
  pb <- progress::progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                        total = length(.x), complete = "=", incomplete = "-",
                                        current = ">", force = TRUE, clear = FALSE, width = 100)
  f <- function(...) {
    pb$tick()
    .f(...)
  }

  suppressWarnings({
    out <- purrr::map(.x, f, ...)
  })

  if (quiet == FALSE) {
    purrr::map(out, "error") %>%
      purrr::walk(message, "\n")
  }

  if (.keep_null == FALSE) {
    out <- out %>%
      keep(~ !is.null(.[["result"]]))
  }

  return(purrr::map(out, "result"))
}
