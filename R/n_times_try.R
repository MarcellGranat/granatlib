n_times_try <- function(.exp, sleep_times = c(3, 3, 3), otherwise = NULL) {
  for (st in sleep_times) {
    tryCatch({out <- {.exp}}, error = \(e) message(crayon::magenta(e), "\n"))
    if (exists("out")) {
      return(out)
      break
    }
    message("Run into error, wait ", crayon::blue(st), " sec")
    Sys.sleep(st)
  }
  message(crayon::red("FATAL"))
  return(otherwise)
}
