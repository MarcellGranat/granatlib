#' @title environment_diff
#'
#' @description Check the difference of the environment between the two state of function call
#'
#' @examples
#' d <- 2
#' e <- 3
#' environment_diff()
#' rm(d)
#' e <- 5
#' a <- 3
#' environment_diff()
#' @export
#'


environment_diff <- function() {

  if (!exists(".global_variables")) {
    .global_variables <<- list()
  } else {
    .global_variables <<- dplyr::last(.global_variables)
  }

  .global_variables <<- ls() |>
    purrr::map(get) |>
    purrr::set_names(ls()) |>
    list() |>
    append(x = .global_variables)

  obj_names <- names(dplyr::last(.global_variables))

  status <- purrr::map_chr(obj_names, \(x) {
    prev = .global_variables[[1]][[x]]
    new = .global_variables[[2]][[x]]

    dplyr::case_when(
      is.null(prev) & !is.null(new) ~ "new",
      !is.null(prev) & is.null(new) ~ "deleted",
      !identical(prev, new) ~ "changed",
      TRUE ~ "unchanged"
    )
  }) |>
    purrr::set_names(obj_names)

  deleted_names <- status |>
    purrr::keep(\(x) x == "deleted") |>
    names()

  if (length(deleted_names) > 0) {
    message(crayon::magenta("DELETED\n"))

    purrr::walk(deleted_names, \(x) {
      message(crayon::magenta(x), "\n")

      capture.output(print(.global_variables[[2]][[x]])) |>
        head(20) |>
        stringr::str_flatten("\n") |>
        (\(msg) message(crayon::red(msg))) ()
    })
  }

  new_names <- status |>
    purrr::keep(\(x) x == "new") |>
    names()

  if (length(new_names) > 0) {
    message(crayon::magenta("NEW\n"))

    purrr::walk(new_names, \(x) {
      message(crayon::magenta(x), "\n")

      capture.output(print(.global_variables[[2]][[x]])) |>
        head(20) |>
        stringr::str_flatten("\n") |>
        (\(msg) message(crayon::green(msg))) ()
    })
  }

  changed_names <-  status |>
    purrr::keep(\(x) x == "changed") |>
    names()

  if (length(changed_names) > 0) {
    message(crayon::magenta("CHANGED\n"))

    purrr::walk(changed_names, \(x) {
      message(crayon::magenta(x), "\n")

      capture.output(print(.global_variables[[1]][[x]])) |>
        head(20) |>
        stringr::str_flatten("\n") |>
        (\(msg) message(crayon::red(msg))) ()

      capture.output(print(.global_variables[[2]][[x]])) |>
        head(20) |>
        stringr::str_flatten("\n") |>
        (\(msg) message(crayon::cyan(msg))) ()
    })
  }

  .global_variables <<- dplyr::last(.global_variables)

}