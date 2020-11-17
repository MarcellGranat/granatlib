#' @title cramer
#'
#' @description Calculates all the pairwised cramer values in a data.frame.
#' @param x Data.frame
#' @param keep.numeric Logical; Should use numeric values as factors and keep them? (FALSE)
#' @param dlimit Integer; If a numeric column has more unique values than this limit, then it will be eliminated(NULL)

#'
#' @return Data.frame, which has 3 column: the compared 2 variables & the value of cramer indicator. \code{overview_print}
#' @examples
#' data(Affairs, package = "AER")
#' cramer(Affairs)
#' cramer(Affairs, keep.numeric = T, dlimit = 7)
#' @export
#'

cramer <- function(x, keep.numeric = F, dlimit = NULL) {

  if (!keep.numeric) x <-  dplyr::select(x, which(!sapply(x, is.numeric)))
  x %>% mutate_at(which(sapply(x, is.numeric)), function(x) as.factor(x))
  x %>% select(which(sapply(x, function(x) !is.numeric(x) | dplyr::n_distinct(x) < dlimit)))

  pedometrics::cramer(data.frame(x))
  y %>% data.frame() %>%
    tibble::rownames_to_column("x") %>%
    pivot_longer(-1, names_to = "y")
}
