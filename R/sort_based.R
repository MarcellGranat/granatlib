#' @title Sort the elements based on a results of a function called on them.
#'
#' @description Sort the elements based on a results of a function called on them.
#'
#' @param x A vector or list.
#' @param .f Called function.
#'
#' @examples
#' sort_based(c("a22", "b33", "c15"), parse_number)
#'

sort_based <- function(x, .f) {
  order <- map_dbl(x, .f) |>
    rank() |>
    enframe(value = "rank") |>
    arrange(rank) |>
    pull(name)

  x[order]
}
