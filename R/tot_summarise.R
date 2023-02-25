#' @title tot_summarise
#'
#' @description Add a total as a new level to the second grouping column. (e.g.: total rate in each industry and in all (2nd col) by years (1st col))
#' @param x A grouped data.frame
#' @param ... Arguments pass to summarise(...)
#' @param total_name Label for the total row. By default it is "Total"
#' @return A grouped data.frame \code{overview_print}
#'
#' @examples
#' iris |>
#'  group_by(Species) |>
#'  tot_summarise(mean(Sepal.Length))
#'
#' @export
#'

tot_summarise <- function(x, ..., total_name = "Total") {

  g_keys <- x |>
    dplyr::group_keys() |>
    names()

  x_ungrouped <- x |>
    dplyr::ungroup()

  if (length(g_keys) > 2) {
    stop("Grouping by max 2 variables are allowed.")
  }

  if (length(g_keys) == 2) {
    x_ungrouped <- x_ungrouped |>
      dplyr::group_by_at(dplyr::vars(g_keys[1]))
  }

  x_ungrouped <- x_ungrouped |>
    dplyr::summarise(...) |>
    dplyr::mutate(g_key = total_name)

  names(x_ungrouped)[which(names(x_ungrouped) == "g_key")] <- dplyr::last(g_keys)

  x_ungrouped <- x_ungrouped |>
    dplyr::select(any_of(g_keys), dplyr::everything()) |>
    dplyr::ungroup()

  bind_rows(
    x |>
      dplyr::summarise(...) |>
      dplyr::ungroup() |>
      dplyr::arrange_at(length(g_keys), desc),
    x_ungrouped
  ) |>
    dplyr::mutate_at(length(g_keys), fct_inorder) |>
    dplyr::group_by_at(g_keys)
}
