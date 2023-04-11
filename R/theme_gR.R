#' @title theme_gR
#'
#' @description A theme that is conditionally dark or minimal (by default). Dark if you use it in RStudio.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Length, Petal.Width)) +
#'   geom_point() +
#'   theme_gR()
#'
#' @export

theme_gR <- function() {

  if (knitr::is_latex_output() | knitr::pandoc_to("docx") | knitr::is_html_output()) {
    ggplot2::theme_get() # only in RStudio
  } else {
    ggdark::dark_theme_minimal() +
      theme(
        plot.background = element_rect(fill = "#161616", color = "#161616"),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "grey25"),
        panel.grid.minor = element_line(color = "grey25"),
        legend.position = "bottom",
        plot.title.position = "plot",
        plot.title = element_text(face = "bold", hjust = .5),
        plot.tag.position = "topright",
        plot.caption.position = "plot",
        legend.key = element_blank(),
        legend.box = "vertical"
      )
  }
}
