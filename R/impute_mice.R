#' @title impute_mice
#'
#' @description using functions from package mice to impute missing data and return the data.frame
#' @param .data Data.frame with missing values
#' @param method method to impute (rf by default)
#' @param ... Additional arguments for mice
#'
#' @return Imputed data.frame \code{overview_print}

#' @export
#'

impute_mice <- function(.data, method = "rf", ...) {
  mice::mice(.data, method = method, printFlag = FALSE, ...) %>%
    mice::complete() %>%
    tibble::tibble()
}

