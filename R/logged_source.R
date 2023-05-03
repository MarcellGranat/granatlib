#' @title Source, but created a .log file in _log folder.
#'
#' @description Rewrites # --- titles in the script with cli messages.
#' @param script Script to run.
#' @export
#'

logged_source <- function(script) {

t <- readLines(script)

purrr::map_chr(t, \(x){
  if (stringr::str_starts(x, "#") & stringr::str_ends(x, "----")) {
    x |>
      stringr::str_remove("#") |>
      stringr::str_remove_all("-") |>
      stringr::str_squish() |>
      stringr::str_c('cli::cli_h1("', `...` = _) |>
      stringr::str_c('")\ncli::cli_alert_info(as.character(Sys.time()))')
  } else {
    x
  }
}) |>
  append('cli::cli_alert_success("End of the script")') |>
  append("cli::cli_alert_info(as.character(Sys.time()))") |>
  append('granatlib::notification()') |>
  cat(file = "temp_r_code.R", sep = "\n")

prev_width <- getOption("width")
options(width = 70)
dir.create("_log", showWarnings = FALSE)
con <- file(paste0("_log/", stringr::str_replace(script, "[.]R", ".log")))
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

# This will echo all input and not truncate 150+ character lines...
source("temp_r_code.R", echo=FALSE, max.deparse.length=10000)

# Restore output to console
sink()
sink(type="message")

options(width = prev_width)

# And look at the log...
cat(readLines(paste0("_log/", stringr::str_replace(script, "[.]R", ".log"))), sep="\n")
unlink("temp_r_code.R")

}

