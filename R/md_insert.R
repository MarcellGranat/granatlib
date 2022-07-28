#' @title md_insert
#'
#' @description Insert a markdown (plain text) code into the rmd file. Use it with inline code!
#'
#' @examples
#' `r md_insert("how_to.md")`
#' @export
#'

md_insert <- function(x, text_contained = NULL) {

  if (str_ends(x, ".md")) {

  if (is.null(text_contained)) { # if not given explicitly

    text_contained = params$text_contained

    if (is.null(text_contained)) { # if not specified in the YAML
      text_contained = TRUE
    }

  }

  if (text_contained) {

    raw_text <- read_delim(x, delim = "+_+_+_+%76324189", col_names = FALSE)[[1]] %>% # read all the lines
      keep(str_starts, "%%", negate = TRUE) %>%
      {ifelse(str_starts(., "#"), str_c("\n", .), .)} %>%
      {str_c(., "\n\n", collapse = "")}
  }
  } else {
    raw_text <- x
  }

  raw_text


  first_character_after <- function(x, ch_after, ch_find) {

  }
# Citation --------------------------------------------------------------------------

  first_character_after <- function(y, ch_after, ch_find, replace_with = "} ") {
    location_after <- str_locate_all(y, ch_after)[[1]][,2]
    location_found <- str_locate_all(y, ch_find)[[1]][,2]

    replace_these <- map(location_after, ~ location_found > .) %>%
      map(which) %>%
      map_dbl(1) %>%
      location_found[.]

    ch_location_shift <- (seq_along(replace_these) - 1) * (str_length(replace_with) - str_length(ch_find))

    replace_these <- replace_these + ch_location_shift

    out <- y

    for (i in replace_these) {
      message(out)
      out <- str_c(str_sub(out, end = i - 1), replace_with, str_sub(out, start = i + 1))
    }

    out
  }

  raw_text %>%
    str_split("\n\n") %>%
    .[[1]] %>%
    first_character_after("\\[@", "")
    str_replace_all("\\[@", "\\\\parencite{") %>%
    str_replace_all("@", "\\\\citet{")
  # TODO citation replace
  # citet!
  # full change if latex

}
