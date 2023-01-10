sort_based <- function(x, .f) {
  order <- map_dbl(x, .f) |>
    rank() |>
    enframe(value = "rank") |>
    arrange(rank) |>
    pull(name)

  x[order]
}
