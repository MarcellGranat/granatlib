#' @title hl
#'
#' @description Manage the different behaviour of html & kable output. Short name (Html-Latex) for markdown inline formatting:  r hl(x, y).
#'
#' @return one of the given outputs based on the environment
#' @examples
#' library(ggplot2)
#' p <- ggplot(iris, aes(Sepal.Length, Petal.Length)) + geom_point()
#' hl(plotly::ggplotly(p), p)
#' @export
#'


hl <- function(output_html, output_latex) {
  if (!knitr::is_latex_output()) {
    output_html
  } else{
    output_latex
  }
}

