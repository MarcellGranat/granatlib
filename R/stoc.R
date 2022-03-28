#' @title stoc
#'
#' @description Same as tictoc::toc, but saves the runtime into a folder with the message
#'
#' @examples
#' tictoc::tic("Example")
#' Sys.sleep(2)
#' stoc()
#' @export
#'

stoc <- function() {
  # save runtime based on tic()-toc() fns to runtime dir
  if (!dir.exists("runtime")) {
    dir.create("runtime")
  }

  out <- capture.output(tictoc::toc()) # save output of tictoc
  message(out)
  if (!is_empty(out)) { # tictoc::tic() req before using
    file_name <- out %>%
      # file_name is taken from tic() msg arg >> required …
      gsub(pattern = ":.*", replacement = ".rds")

    if (str_starts(file_name, "\\d") | file_name == "") warning("Empty tictoc msg!") # ^
    file_name <- str_c("runtime/runtime_", file_name)
    saveRDS(out, file_name)
  } else {
    warning(crayon::bgRed("No tic() in current session!"))
  }
}
