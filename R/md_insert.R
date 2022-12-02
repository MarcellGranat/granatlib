#' @title md_insert
#'
#' @description Insert a markdown (plain text) code into the rmd file. Use it with inline code!
#'
#' @examples
#' `r md_insert("how_to.md")`
#' @export
#'


md_insert <- function(x, text_contained = NULL, asis = TRUE, fig_captions = NULL, tab_captions = NULL, todo_contained = NULL) {
  library(tidyverse)

  if (is.null(text_contained)) { # if not given explicitly

    text_contained = params$text_contained

    if (is.null(text_contained)) { # if not specified in the YAML
      text_contained = TRUE
    }

  }

    if (is.null(todo_contained)) { # if not given explicitly

      todo_contained = params$todo_contained

    if (is.null(todo_contained)) { # if not specified in the YAML
      todo_contained = TRUE
    }
    }

  remove_lines <- "%%|#\\w"

  if (!todo_contained) {
    remove_lines <- remove_lines %>%
      str_c("|- [ ]|- [x]")
  }

  if (text_contained) {

    if (str_ends(x, ".md")) {
      suppressMessages({
        out <- read_delim(x, delim = "+_+_+_+%76324189", col_names = FALSE)[[1]] %>% # read all the lines
          keep(str_starts, remove_lines, negate = TRUE) %>%
          {ifelse(str_starts(., "#"), str_c("\n", .), .)}
      })

      # eq lines
      eq_begin <- str_detect(out, "begin[{]equation") %>%
        which()
      eq_end <- str_detect(out, "end[{]equation") %>%
        which()

      not_eq_lines <- seq_along(out)

      if (length(eq_begin) != 0) {
        eq_lines <- map2(eq_begin, eq_end, ~ seq(from = .x, to = .y - 1)) %>%
          reduce(c) %>%
          unique()

        not_eq_lines <- not_eq_lines %>%
          setdiff(eq_lines)
      }

      for (i in not_eq_lines) {
        out[i] <- str_c(out[i], "\n")
      }

      out <- str_flatten(out, collapse = "\n")

    } else {

      out <- x
    }

    if (str_detect(out, "@")) { # reference

      if (is.null(fig_captions)) {
        fig_captions <- list.files(all.files = TRUE, full.names = TRUE, recursive = T) %>%
          keep(str_detect, "fig_captions")
      }

      if (is.null(tab_captions)) {
        tab_captions <- list.files(all.files = TRUE, full.names = TRUE, recursive = T) %>%
          keep(str_detect, "tab_captions")
      }

      captions_df <- fig_captions %>%
        read_lines() %>%
        enframe() %>%
        mutate(
          id = cumsum(str_starts(value, "label:")),
          name = case_when(
            str_starts(value, "label") ~ "label",
            str_starts(value, "caption") ~ "caption",
            str_starts(value, "note") ~ "note",
            TRUE ~ as.character(NA)
          ),
          value = ifelse(!is.na(name), str_remove(value, str_c(name, ": ")), value)
        ) %>%
        fill(name) %>%
        filter(value != "")

      # ref

      latex_labels <- captions_df %>%
        filter(name == "label") %>%
        pull(value) %>%
        unique()

      tab_captions_df <- tab_captions %>%
        read_lines() %>%
        enframe() %>%
        mutate(
          id = cumsum(str_starts(value, "label:")),
          name = case_when(
            str_starts(value, "label") ~ "label",
            str_starts(value, "caption") ~ "caption",
            str_starts(value, "note") ~ "note",
            TRUE ~ as.character(NA)
          ),
          value = ifelse(!is.na(name), str_remove(value, str_c(name, ": ")), value)
        ) %>%
        fill(name) %>%
        filter(value != "")

      # ref

      tab_labels <- tab_captions_df %>%
        filter(name == "label") %>%
        pull(value) %>%
        unique()

      total_labels <- c(latex_labels, tab_labels, "eq:")

      out <- granatlib::replace_references(out, total_labels = total_labels)
    }


    if (asis) {
      knitr::asis_output(out)
    } else {
      out
    }
  }

}
