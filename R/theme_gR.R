#' @title theme_gR
#'
#' @description A theme that is conditionally dark or minimal (by default). Dark if you use it in RStudio.
#'
#' @param base_theme A theme to use
#'
#' @examples
#' ggplot(iris, aes(Sepal.Length, Petal.Width)) +
#'   geom_point() +
#'   theme_gR()
#'
#' @export

theme_gR <- function(base_theme = NULL) {

  if (is.null(base_theme)) {
    if (.Platform$GUI == "RStudio") {
      base_theme <- ggdark::dark_theme_minimal() +
        theme(
          plot.background = element_rect(fill = "#161616", color = "#161616"),
          panel.border = element_blank(),
          panel.grid.major = element_line(color = "grey25"),
          panel.grid.minor = element_line(color = "grey25")
        )
    } else {
      base_theme <- ggplot2::theme_minimal()
    }
  }

  base_theme +
    theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", hjust = .5),
      plot.tag.position = "topright",
      plot.caption.position = "plot",
      legend.key = element_blank(),
      legend.box = "vertical"
    )
}
