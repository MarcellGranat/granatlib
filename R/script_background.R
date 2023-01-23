script_background <- function(script_name = NULL, name = NULL, ..., remove_temp_code = TRUE, sleep_time = 7) {

if (is.null(name)) {
  name <- script_name %>%
    str_remove("[.]R") %>%
    str_replace_all("-", " ") %>%
    str_remove("\\d* ") %>%
    str_to_title()
}

if (is.null(script_name)) {
  stop("Provide an R script name.")
}

temp_file_name <- str_remove_all(Sys.time(), "\\D") %>%
  str_c("_temp_code.R")

temp_code <- str_c('
if ("utils.R" %in% list.files()) {
  source("utils.R")
} else {
  suppressPackageStartupMessages({
    library(tidyverse)
  })
}
message(crayon::bgMagenta("Running ", "',script_name,'" ))
tictoc::tic(msg = str_remove("',script_name,'", "[.]R"))
tryCatch({
  source("',script_name,'")
}, error = function(e) {
 e <<- e
})

if (exists("e")) {
  suppressWarnings({
    notification(msg = as.character(e), sound = TRUE)
  })
  stop(as.character(e), call. = FALSE)
}

toc_msg <- capture.output(tictoc::toc())
notification(msg = toc_msg, sound = TRUE)
message(crayon::bgGreen("Succeeded."))
message(crayon::bgCyan(toc_msg))
rm("toc_msg")
')

cat(temp_code, file = temp_file_name)

rstudioapi::jobRunScript(temp_file_name, name = name, ...)

if (remove_temp_code) {
  Sys.sleep(sleep_time)
  file.remove(temp_file_name)
}

}
