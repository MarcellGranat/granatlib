cache_map <- function(.x, .f, ..., .id = NULL, msg = NULL, .board = NULL, n_savepoint = 100, notification = TRUE) {

  if (length(.x) < n_savepoint) {
    n_savepoint <- length(.x)
  }

  if (is.null(msg)) {
    if (!is.null(.id)) {
      msg <- .id
    }
    msg <- deparse(substitute(.f))
  }

  if (is.null(.id)) {
    message(".id is suggested")
    .id <- msg |>
      str_remove_all("\\W") |>
      str_to_lower() |>
      str_flatten("")
  }

  if (is.null(.board)) {
    if (str_c(.id, ".rds") %in% list.files()) {
      out <- read_rds(str_c(.id, ".rds"))
    } else {
      out <- list(data = c(), k = 0)
    }

  } else {
    if (.id %in% pin_list(.board)) {
      out <- pin_read(.board, .id)
    } else {
      out <- list(data = c(), k = 0)
    }
  }

  q <- as.numeric(floor(quantile(seq_along(.x), probs = 1:n_savepoint / n_savepoint)))

  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                        total = length(.x) - c(0, q)[out$k + 1], complete = "=", incomplete = "-",
                                        current = ">", force = TRUE, clear = FALSE, width = 100)

  f <- function(...) {
    pb$tick()

    .f(...)
  }

  if (out$k != n_savepoint) {
    message(crayon::magenta(msg))

    for (k in seq(from = out$k + 1, to = n_savepoint)) {

      out <- list(
        data = c(out$data, purrr::map(.x[seq(from = c(0, q)[k] + 1, to = q[k])], f, ...)),
        k = k
      )

      if (is.null(.board)) {
        write_rds(out, file = str_c(.id, ".rds"))
      } else {
        pin_write(.board, out, .id)
      }

      if (notification) {
        granatlib::notification(str_c("Saving: ", .id ," (", scales::percent(out$k / n_savepoint), ")"))
      }

    }
  }

  return(out$data)
}
