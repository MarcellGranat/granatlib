n_times_try <- function(.exp, sleep_times = c(3, 3, 3), otherwise = NULL, print_warning = FALSE) {
  n_try <- 0
  done <- FALSE
  while (!done) {
    out <- tryCatch({
      done <- TRUE
      .exp
    }, error = function(e) {
      if (print_warning) {
        warning(e)
      }
      assign("done", FALSE, envir = parent.frame())
    })

    if (!done) {
      n_try <- n_try + 1
      if (n_try <= length(sleep_times)) {
        if (print_warning) {
          message("Run into error, wait ", crayon::blue(sleep_times[n_try]), " sec")
          Sys.sleep(sleep_times[n_try])
        }
      } else {
        warning("Fatal")
        done <- TRUE
        return(otherwise)
      }
    }
  }

  if (n_try < length(sleep_times)) {
    return(out)
  }
}
