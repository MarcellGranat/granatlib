#' @title depedency_check
#'
#' @description Check the dependencies of a function and compare it with the import field. Also possible to rewrite the code with explicit function calls.
#'
#' @export
#'


depedency_check <- function(file_name) {

  code <- readLines(file_name)

  fns <- stringr::str_extract_all(code, "[A-Z,a-z][A-Z,a-z,0-9,.,_]+\\(")
  fns <- unlist(fns)
  fns <- stringr::str_replace(fns, "\\(", "")
  fns

  process_getAnywhere <- function(ga){
    where <- ga$where[grepl("package", ga$where)]
    where <- sub("package[:]", "", where)
    where
  }

  pkg_df <- do.call("rbind",
                    lapply(fns,
                           function(f){
                             ga <- process_getAnywhere(getAnywhere(f))
                             data.frame(fn = rep(f, length(ga)),
                                        pkg = ga,
                                        stringsAsFactors = FALSE)
                           }
                    ))

  if ("DESCRIPTION" %in% list.files()) {
    imported <- readr::read_lines("DESCRIPTION") |>
      (\(x) {
        st <- which(x == "Imports:")
        en <- purrr::keep(which(!stringr::str_starts(x, " ")), ~ . > st)
        stringr::str_squish(x[(st + 1):(en - 1)])
      }) ()

    unimported <- pkg_df |>
      dplyr::distinct(pkg) |>
      dplyr::anti_join(tibble::tibble(pkg = imported), by = "pkg") |>
      dplyr::pull(pkg) |>
      dplyr::setdiff(c("base", "utils"))

    if (length(unimported) > 0) {

      cat(crayon::red("Not imported:\n"))
      purrr::walk(unimported, ~ cat(" ", ., ",\n"))
    }
  }

  pkg_df <- pkg_df |>
    mutate(
      pkg = ifelse(fn == "tibble", "tibble", pkg)
    ) |>
    dplyr::filter(pkg != "base" & pkg != "utils") |>
    dplyr::distinct()

  for (i in 1:nrow(pkg_df)) {
    code <- str_replace_all(code, pattern = pkg_df[i, 1], stringr::str_c(pkg_df[i, 2], "::", pkg_df[i, 1]))
  }

  created_fun_chr <- str_extract(code, "[A-z, 0-9]*::.* <-") |>
    na.omit()

  for (i in created_fun_chr) {
    code <- str_replace(code, i, str_remove(i, ".*::"))
  }

  for (i in 1:4) {
    code <- stringr::str_replace_all(code, "::[A-z,0-9,.]*::", "::")
  }


  code <- stringr::str_replace_all(code, "@title .*::", "@title ")

  name_of_func <- stringr::str_remove_all(file_name, "R/|[.]R")
  code <- stringr::str_replace_all(code, paste0(" .*::", name_of_func, "[(]"), paste0(" ",name_of_func, "("))

  cat(code, sep = "\n")

  message("\nOverwrite? y/n")
  ans = readline()
  if (ans == "y") {

    cat(stringr::str_flatten(code, "\n"), file = file_name)
    message(crayon::bgGreen(file_name, " modified!"))
  } else {
    message(crayon::bgRed(file_name, " untouched!"))
  }

}
