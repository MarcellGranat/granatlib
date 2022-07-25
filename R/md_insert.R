#' @title md_insert
#'
#' @description Insert a markdown (plain text) code into the rmd file. Use it with inline code!
#'
#' @examples
#' `r md_insert("how_to.md")`
#' @export
#'

md_insert <- function(md_name, text_contained = NULL) {

  if (is.null(text_contained)) {
    if (exists("params$text_contained")) {
      text_contained = params$text_contained
    } else {
      text_contained = TRUE
    }
  }

  if (text_contained) {

  read_delim(md_name, delim = "+_+_+_+%76324189", col_names = FALSE)[[1]] %>% # read all the lines
    {ifelse(str_starts(., "#"), str_c("\n", .), .)} %>%
    {str_c(., "\n\n", collapse = "")}
  }

  # TODO citation replace
    # citet!
    # full change if latex
}
