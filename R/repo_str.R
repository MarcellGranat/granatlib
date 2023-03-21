#' @title Structure the repository based on Marcell's preferences.
#'
#' @description Adds README, utils, board and .Rprofile files and updates them.
#' @examples
#' repo_str()
#' @export
#'

repo_str <- function(board = TRUE) {

  if (!".gitignore" %in% list.files(all.files = TRUE)) {
    cat(c("*", "![-09][0-9]*.R", "!README.md", "!logo.png", ".Rproj.user"), file = ".gitignore", sep = "\n")
    granatlib::info("{'.gitignore'} created", "ok", add_time = FALSE)

  } else if (!"*" %in% readLines(".gitignore")) {
    cat(c("*", "![-09][0-9]*.R", "!README.md", "!logo.png", ".Rproj.user"), file = ".gitignore", sep = "\n")
    granatlib::info("{'.gitignore'} updated", "warning", add_time = FALSE)

  }

  utils_filename <- list.files() |>
    purrr::keep(stringr::str_detect, pattern = "utils[.]R") |>
    dplyr::first()

  if (is.na(utils_filename)) {
    cat(c('if (!require(pacman, quietly = TRUE)) install.packages("pacman"); library(pacman)',
          'p_load("tidyverse", "pins", "currr")',
          'p_load_gh("marcellgranat/granatlib")',
          'p_load_gh("marcellgranat/ggProfessional")',
          '',
          'options(currr.folder = ".currr", currr.fill = FALSE)',
          '',
          'theme_set(theme_gR())'
    ), file = "00-utils.R", sep = "\n")

    utils_filename <- "00-utils.R"

    granatlib::info("{'00-utils.R'} created", "ok", add_time = FALSE)
  }

  if (board) {

    if (!"00-board.R" %in% list.files()) {
      cat(
        paste0(
          '.board <- board_ms365( # You should update this based on your permission to the data
  drive = Microsoft365R::get_business_onedrive("common"),
  path = "', stringr::str_remove(rstudioapi::getActiveProject(), ".*/"), '",
  versioned = FALSE
)

.write <- function(..., name = NULL) {
  l <- list(...)
  if (is.null(name)) {
    name <- deparse(substitute({{ ... }})) |>
      head(-2) |>
      tail(-2) |>
      str_squish()
  }

  purrr::walk2(l, name, \\(x, y) {
      pin_write(
        board = .board,
        name = y,
        x
      )
  })
}

.read <- function(...) {
    name <- deparse(substitute({{ ... }})) |>
      head(-2) |>
      tail(-2) |>
      str_squish()

  purrr::walk(name, \\(x) {
    pin_read(
      board = .board,
      name = x
    ) |>
      assign(x = x, pos = 1)
  })
}'), file = "00-board.R")
    }
  }

if (!".Rprofile" %in% list.files(all.files = TRUE)) {
  cat(c("library(stats)", paste0('source("', utils_filename,'")'), ifelse(board, 'source("00-board.R")', ''), "paint::mask_print()", "options(paint_palette = paint::brewer_dark2_7())"), file = ".Rprofile", sep = "\n")
  granatlib::info("{'.Rprofile'} created", "ok", add_time = FALSE)
}

if ("functions" %in% list.dirs() & !'walk(list.files("functions", full.names = TRUE), source)' %in% readLines(utils_filename)) {
  readLines(utils_filename, warn = FALSE) |>
    append("") |>
    append('walk(list.files("functions", full.names = TRUE), source)') |>
    cat(utils_filename, sep = "\n")

  granatlib::info("{functions} added to the utils file")
}

if (!"README.md" %in% list.files()) {
  cat('# ', stringr::str_remove(rstudioapi::getActiveProject(), ".*/"), file = "README.md")

  granatlib::info("{'README.md'} created", "ok", add_time = FALSE)
}

readme <- readLines("README.md", warn = FALSE)

if ("logo.png" %in% list.files() & !stringr::str_detect(readLines("README.md", warn = FALSE)[1], "img src")) {
  readme[1] <- paste(readme[1], '<img src="logo.png" align="right" width="120" height="140"/>')

  granatlib::info("{'logo'} added to README.md", "ok", add_time = FALSE)
}

if (!"### Files of the repository" %in% readme) {
  readme <- readme |>
    append("") |>
    append("### Files of the repository") |>
    append("") |>
    append("") |>
    append("")
}

last_fileline <- which(stringr::str_starts(readme, "#")) |>
  purrr::keep(~ . > which(readme == "### Files of the repository")) |>
  dplyr::first() |>
  (\(x) x - 2) ()

if (is.na(last_fileline)) last_fileline <- which(readme == "### Files of the repository") + 2


file_names <- list.files(pattern = "[0-9][0-9].*R") |>
  purrr::discard(stringr::str_detect, pattern = "utils[.]R") |>
  sort() |>
  append(utils_filename, 0) |>
  rev()

for (i in seq_along(file_names)) {

  l <- paste0("    ├── ", file_names[i])
  if (l %in% readme) {
    break
  }

  de <- dplyr::case_when(
    file_names[i] == utils_filename ~ "Prior to evaluating any other scripts, it is recommended to execute this particular script, which is responsible for loading the necessary packages and options.",
    file_names[i] == "00-board.R" ~ "The enclosed data was loaded and exported into a private OneDrive folder. Additionally, all intermediate results were saved in the same folder using the [{pins}](https://pins.rstudio.com) package. If you would like access to the folder, please email [granat.marcell\\@uni-neumann.hu](mailto:granat.marcell@uni-neumann.hu). There is a chance that you do not have access to the folder, or that you have but in another way. In this case, it may be necessary that the `.board` object created in the script must be modified accordingly. Similarly to the utils file, it is recommended to execute this script prior to any other.",
    TRUE ~ "..."
  )

  if (i == 1) {
    readme <- append(readme, values = c("", l, "", de, ""), after = last_fileline - 1)
  }  else {
    readme <- append(readme, values = c("", l, "", de, ""), after = which(readme == paste0("    ├── ", file_names[i - 1]))[1] - 1)
  }
  granatlib::info("{'A new file'} added to the 'Files of the repository' s. in the README", add_time = FALSE)
}

if (!identical(readme, readLines("README.md", warn = FALSE))) {
  cat(readme, file = "README.md", sep = "\n")

  granatlib::info("{'README.md'} updated", add_time = FALSE)
}

}

