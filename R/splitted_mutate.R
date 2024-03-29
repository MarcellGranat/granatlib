#' @title splitted_mutate
#'
#' @description Splits the data.frame into separeta parts and evaluates it one-by-one. Useful for saving memmory during large computations.
#' @param .data Data frame.
#' @param ... arguments to pass mutate(...).
#' @param split_number Number of parts to split the data.frame before evaluation.
#' @param .keep Variables to keep after eval the expression. Original columns are keept automaticly.
#' @param cores Number of cores to use. If cores = 1 (default) then evaluation is made by simple for loop.
#' @return .data \code{overview_print}
#' @examples
#'
#'iris_samples <- tibble(data = replicate(100, sample_n(iris, 50), simplify = FALSE))
#'
#'iris_samples
#'
#'iris_samples %>%
#'  splitted_mutate(
#'    fit = map(data, lm, formula = Sepal.Length ~ Sepal.Width),
#'    coefs = map(fit, "coefficients"),
#'    coef1 = map_dbl(coefs, 1),
#'    coef2 = map_dbl(coefs, 2),
#'    .keep = c("coef1", "coef2") # single core
#'  )
#'
#'iris_samples %>%
#'  splitted_mutate(
#'    fit = map(data, lm, formula = Sepal.Length ~ Sepal.Width),
#'    coefs = map(fit, "coefficients"),
#'    coef1 = map_dbl(coefs, 1),
#'    coef2 = map_dbl(coefs, 2),
#'    .keep = c("coef1", "coef2"),
#'    cores = 6 # multicore evaluation
#'  )
#'
#' @export
#'


splitted_mutate <- function(.data, ..., split_number = 100, .keep = NULL, .progress = TRUE) {
  if (nrow(.data) < split_number) {
    split_number <- floor(nrow(.data) / 5)
  }

  if (split_number < 1) {
    split_number <- 1
  }

  out <- tibble()

  if (.progress) {
    granatlib::create_pb(split_number, .message = FALSE)
  }

  for (i in 1:split_number) {
    current_result <- .data %>%
      filter(cut(row_number(), split_number, FALSE) == i) %>%
      mutate(
        ...
      )

    if (!is.null(.keep)) { # remove cols >> save memory
      current_result <- current_result %>%
        select(names(.data), .keep)
    }

    out <- bind_rows(out, current_result)

    if (.progress) {
      pb$tick()
    }
  }

  out
}



