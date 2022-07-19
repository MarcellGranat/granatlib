#' @title md_insert
#'
#' @description Insert a markdown (plain text) code into the rmd file. Use it with inline code!
#'
#' @examples
#' `r md_insert("how_to.md")`
#' @export
#'

md_insert <- function(md_name) {
  read_delim(md_name, delim = "++++", col_names = FALSE)[[1]] %>%
    {ifelse(str_starts(., "#"), str_c("\n", .), .)} %>%
    {str_c(., "\n\n", collapse = "")}

  # TODO citation replace
}
