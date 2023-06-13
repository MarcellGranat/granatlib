#' @title Structure the repository based on Marcell's preferences.
#'
#' @description Adds README, utils, board and .Rprofile files and updates them.
#' @param board logical. Should a script for {pins} added?
#' @param chatgpt logical. Should chatGPT add comments to the scripts?
#' @examples
#' repo_str()
#' @export
#'

repo_str <- function(board = TRUE, chatgpt = TRUE) {

  cli::cli_h1("Structuriing the repository")

  if (!".gitignore" %in% list.files(all.files = TRUE)) { # exclude everything except numbered .R files (01-data.R)
    cat(c("*", "![-09][0-9]*.R", "!README.md", "!logo.png", ".Rproj.user"), file = ".gitignore", sep = "\n")
    granatlib::info("{'.gitignore'} created", "ok", add_time = FALSE)
    cli::cli_alert_success(".gitignore created")

  } else if (!"*" %in% readLines(".gitignore")) {
    cat(c("*", "![-09][0-9]*.R", "!README.md", "!logo.png", ".Rproj.user"), file = ".gitignore", sep = "\n") # also README and logo
    cli::cli_alert_info(".gitignore updated")
  }

  utils_filename <- list.files() |> # check if utils exists
    purrr::keep(stringr::str_detect, pattern = "utils[.]R") |>
    dplyr::first()

  if (is.na(utils_filename)) { # my current utils set
    cat(c('if (!require(pacman, quietly = TRUE)) install.packages("pacman"); library(pacman)',
          'p_load("tidyverse", "pins", "currr")',
          'p_load_gh("marcellgranat/granatlib")',
          'p_load_gh("marcellgranat/ggProfessional")',
          '',
          'options(currr.folder = ".currr", currr.fill = FALSE)',
          '',
          'theme_set(',
          '\ttheme_bw() +',
          '\t\ttheme(',
          '\t\t\tlegend.position = "bottom"',
          '\t\t)',
          ')'
    ), file = "00-utils.R", sep = "\n")

    utils_filename <- "00-utils.R"

    cli::cli_alert_success("00-utils.R created")
  }

  if (board) { # .read and .write files

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

if (!".Rprofile" %in% list.files(all.files = TRUE)) { # my current .Rprofile setup
  cat(c("library(stats)", paste0('source("', utils_filename,'")'), ifelse(board, 'source("00-board.R")', ''), "ggplot2::theme_set(granatlib::gR_theme())",
        'Sys.setenv("OPENAI_API_KEY" = readLines(paste0(path.package("granatlib"), "/openai_api_key"), warn = FALSE)) # for package {chatgpt}'
        ), file = ".Rprofile", sep = "\n")
  cli::cli_alert_success(".Rprofile created")
}

if ("functions" %in% list.dirs() & !'walk(list.files("functions", full.names = TRUE), source)' %in% readLines(utils_filename)) {
  readLines(utils_filename, warn = FALSE) |>
    append("") |>
    append('walk(list.files("functions", full.names = TRUE), source)') |>
    cat(utils_filename, sep = "\n")

  cli::cli_alert_success("functions added to the utils file")
}

cli::cli_h2("README") # update README

if (!"README.md" %in% list.files()) {
  cat('# ', stringr::str_remove(rstudioapi::getActiveProject(), ".*/"), file = "README.md")

  cli::cli_alert_success("README.md created")
}

readme <- readLines("README.md", warn = FALSE)

if ("logo.png" %in% list.files() & !stringr::str_detect(readLines("README.md", warn = FALSE)[1], "img src")) {
  readme[1] <- paste(readme[1], '<img src="logo.png" align="right" width="120" height="140"/>')

  cli::cli_alert_info("logo.png added to the README")
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

spinny <- cli::make_spinner(
  which = "dots2",
  template = "\033[35m{spin}\033[39m Generating comments to your \033[35mREADME.md\033[39m"
)

for (i in seq_along(file_names)) {

  spinny$spin()
  l <- paste0("    ├── ", file_names[i])
  if (l %in% readme) {
    break
  }

  de <- dplyr::case_when(
    file_names[i] == utils_filename ~ "Prior to evaluating any other scripts, it is recommended to execute this particular script, which is responsible for loading the necessary packages and options.",
    file_names[i] == "00-board.R" ~ "The enclosed data was loaded and exported into a private OneDrive folder. Additionally, all intermediate results were saved in the same folder using the [{pins}](https://pins.rstudio.com) package. If you would like access to the folder, please email [granat.marcell\\@uni-neumann.hu](mailto:granat.marcell@uni-neumann.hu). There is a chance that you do not have access to the folder, or that you have but in another way. In this case, it may be necessary that the `.board` object created in the script must be modified accordingly. Similarly to the utils file, it is recommended to execute this script prior to any other.",
    TRUE ~ "..."
  )


  if (de == "..." & chatgpt) { # generate standard description with ChatGPT
    if (Sys.getenv("OPENAI_API_KEY") == "") {
      stop('You need add your OPENAI_API_KEY: Sys.setenv("OPENAI_API_KEY" = "xxx"')
    }
    tryCatch({
      q <- stringr::str_flatten(c("Describe what this code does. This will go to the README file of the repository, so it should be technical, but short. Use maximum 5 sentences. It should not contain codes from the file and do not use linebreaks or bullet points. If you refer to an object or function, put it in `` (e.g. `mean()`).:", readLines(file_names[i])), collapse = "\n")
      capture.output({ # avoid printing
        de <- chatgpt::ask_chatgpt(question = q)
      })
    }, error = \(e) cli::cli_alert_danger(e))

  }

  if (i == 1) {
    readme <- append(readme, values = c("", l, "", de, ""), after = last_fileline - 1)
  }  else {
    readme <- append(readme, values = c("", l, "", de, ""), after = which(readme == paste0("    ├── ", file_names[i - 1]))[1] - 1)
  }

}

spinny$finish()

if (!identical(readme, readLines("README.md", warn = FALSE))) {
  cat(readme, file = "README.md", sep = "\n")

  cli::cli_alert_success("README updated!")
}

}

