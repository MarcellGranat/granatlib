#' @title Call TryCatch n times with given sleep times.
#'
#' @description Mainly suggested for web scraping when reaching a given domain in a short time leads to errors.
#' @param .exp Expression
#' @param sleep_time Numeric vector given the seconds to wait for next try.
#' @param otherwise Value to return if all the tries resulted an error.
#' @param print_message Should print the message (default FALSE).
#'
#' @examples
#' v <- list("a", "B", 2) # only 3rd element can be added to another numeric
#' i = 0
#' n_times_try({
#' i = i + 1
#' v[[i]] + 3
#' })

n_times_try <- function(.exp, sleep_times = c(3, 3, 3), otherwise = NULL, print_message = FALSE) {
  for (st in sleep_times) {
    tryCatch({out <- {.exp}}, error = \(e) if (print_message) message("\n", crayon::magenta(e)))
    if (exists("out")) {
      return(out)
      break
    }
    if (print_message) {
      message("Wait ", crayon::blue(st), " sec")
    }
    Sys.sleep(st)
  }
  message(crayon::red("FATAL"))
  return(otherwise)
}
