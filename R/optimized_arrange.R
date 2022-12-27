optimized_arrange <- function(x, n) {

  if (is.data.frame(x)) {
    remain <- seq(nrow(x))
  }  else {
    remain = seq_along(x)
  }

  new_order <- c()

  while (length(remain) != 0) {
    new_order <- c(new_order, head(remain, n), rev(tail(remain, n)))
    remain <- base::setdiff(remain, new_order)
  }

  if (is.data.frame(x)) {
    x[new_order, ]
  } else {
    x[new_order]
  }

}
