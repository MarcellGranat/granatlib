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
