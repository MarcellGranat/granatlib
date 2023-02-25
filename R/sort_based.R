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
#' @export

sort_based <- function(x, .f) {
  order <- purrr::map_dbl(x, .f) |>
    rank() |>
    tibble::enframe(value = "rank") |>
    dplyr::arrange(rank) |>
    dplyr::pull(name)

  x[order]
}
