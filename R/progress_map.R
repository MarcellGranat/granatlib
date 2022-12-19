progress_map <- function(.x, .f, ..., .id = NULL, msg = NULL) {
    if (is.null(msg)) {
      msg <- deparse(substitute(.f))
    }
    message(crayon::magenta(msg))
    .f <- purrr::as_mapper(.f, ...)

    pb <- progress::progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                          total = length(.x), complete = "=", incomplete = "-", current = ">",
                                          force = TRUE, clear = FALSE, width = 100)

    f <- function(...) {
      pb$tick()
      .f(...)
    }
    purrr::map(.x, f, ...)
}
