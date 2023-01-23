cb_background <- function(name = NULL, remove_temp_code = FALSE, sleep_time = 7) {

  if (is.null(name)) {
    name <- clipr::read_clip() %>%
      keep(str_detect, "\\w") %>%
      str_remove("<-.*") %>%
      first()
  }

  temp_file_name <- str_remove_all(Sys.time(), "\\D") %>%
    str_c("_temp_code.R")

  temp_code <- str_c('
library(tidyverse)
if ("utils.R" %in% list.files()) {
  source("utils.R")
  message("Run utils.R")
}
message(crayon::bgMagenta("Running code from clipboard"))
message(crayon::cyan(str_flatten(clipr::read_clip(allow_non_interactive = TRUE), "\n")))
tictoc::tic()
tryCatch({
',
  stringr::str_flatten(clipr::read_clip(allow_non_interactive = TRUE), "\n"),
'
}, error = function(e) {
 e <<- e
})

if (exists("e")) {
  suppressWarnings({
    granatlib::notification(msg = as.character(e), sound = TRUE)
  })
  stop(as.character(e), call. = FALSE)
}

toc_msg <- capture.output(tictoc::toc())
granatlib::notification(msg = toc_msg, sound = TRUE)
message(crayon::bgGreen("Succeeded."))
message(crayon::bgCyan(toc_msg))
rm("toc_msg")
')

  cat(temp_code, file = temp_file_name)

  rstudioapi::jobRunScript(
    temp_file_name,
    name = name,
    importEnv = TRUE,
    exportEnv = "R_GlobalEnv"
    )

  if (remove_temp_code) {
    Sys.sleep(sleep_time)
    file.remove(temp_file_name)
  }

}
