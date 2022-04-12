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


splitted_mutate <- function(.data, ..., split_number = 100, .keep = NULL, cores = 1) {
  if (nrow(.data) < split_number) {
    split_number <- floor(nrow(.data) / 5)
  }

  if (split_number < 1) {
    split_number <- 1
  }

  out <- tibble()

  granatlib::create_pb(n = split_number)

  if (cores == 1) {

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
      out <- bind_rows(
        out, current_result
      )

      pb$tick()
    }
  } else {

    dots <- list(...)

    l <- .data %>%
      mutate(g = cut(row_number(), split_number, FALSE)) %>%
      group_by(g) %>%
      group_split(.keep = FALSE)

    cl <- makeCluster(cores)

    clusterEvalQ(cl, expr = library(tidyverse))
    clusterEvalQ(cl, expr = library(stringr))
    clusterEvalQ(cl, expr = library(dplyr))
    clusterEvalQ(cl, expr = library(tidyr))
    clusterEvalQ(cl, expr = library(purrr))
    clusterEvalQ(cl, expr = library(tibble))
    clusterEvalQ(cl, expr = library(forcats))

    clusterExport(cl=cl, varlist=c("l", "dots", ".keep", ".data", "pb"), envir=environment())

    out <- parallel::parLapply(cl, X = l,  function(x) {

      mutate(x, ...)

      if (!is.null(.keep)) { # remove cols >> save memory
        current_result <- current_result %>%
          select(names(.data), .keep)
      }
      pb$tick()
    })
    stopCluster(cl)
    out <- bind_rows(out)
  }

  out
}



