#' @title tot_summarise
#'
#' @description Add a total as a new level to the second grouping column. (e.g.: total rate in each industry and in all (2nd col) by years (1st col))
#' @param x A grouped data.frame
#' @param ... Arguments pass to summarise(...)
#' @param total_name Label for the total row. By default it is "Összesen"
#' @return A grouped data.frame \code{overview_print}
#' @export
#'

tot_summarise <- function(x, ..., total_name = "Összesen") {

  g_keys <- x %>%
    group_keys() %>%
    names()

  x_ungrouped <- x %>%
    ungroup()

  if (length(g_keys) > 2) {
    stop("Grouping by max 2 variables are allowed.")
  }

  if (length(g_keys) == 2) {
    x_ungrouped <- x_ungrouped %>%
      group_by_at(vars(g_keys[1]))
  }

  x_ungrouped <- x_ungrouped %>%
    summarise(...) %>%
    mutate(g_key = total_name)

  names(x_ungrouped)[which(names(x_ungrouped) == "g_key")] <- last(g_keys)

  x_ungrouped <- x_ungrouped %>%
    select(g_keys, everything()) %>%
    ungroup()

  bind_rows(
    x_ungrouped,
    x %>%
      summarise(...) %>%
      ungroup() %>%
      arrange_at(length(g_keys), desc)
  ) %>%
    mutate_at(length(g_keys), fct_inorder) %>%
    group_by_at(g_keys)
}
