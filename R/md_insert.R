#' @title md_insert
#'
#' @description Insert a markdown (plain text) code into the rmd file. Use it with inline code!
#'
#' @examples
#' `r md_insert("how_to.md")`
#' @export
#'

md_insert <- function(x, text_contained = NULL, asis = TRUE) {


  if (is.null(text_contained)) { # if not given explicitly

    text_contained = params$text_contained

    if (is.null(text_contained)) { # if not specified in the YAML
      text_contained = TRUE
    }

  }

  if (text_contained) {

    if (str_ends(x, ".md")) {
      out <- read_delim(x, delim = "+_+_+_+%76324189", col_names = FALSE)[[1]] %>% # read all the lines
        keep(str_starts, "%%", negate = TRUE) %>%
        {ifelse(str_starts(., "#"), str_c("\n", .), .)} %>%
        {str_c(., "\n\n", collapse = "")}
    }else {
      out <- x
    }
    if (asis) {
      knitr::asis_output(out)
    } else {
      out
    }
  }

}
