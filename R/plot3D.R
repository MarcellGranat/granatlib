#' @title plot3D
#'
#' @description Plot a function with 2 input values in 3D based on ggplot2 & rayshader package.
#' @param f The function, which will be plotted in 3D (function(x, y) x^0.2*y^0.2)
#' @param x.min Lower limit of x-axis (0)
#' @param y.min Lower limit of y-axis (0)
#' @param x.max Upper limit of x-axis (10)
#' @param y.max Upper limit of y-axis (10)
#' @param n Number of points in rows and columns (100)
#' @param low Colour of the lowest value ("blue")
#' @param high Colour of the highest value ("red")
#' @param title Title of the plot (f as expression)
#' @param legend.title Title of the legend ("f")
#' @param xlab Title of the x-axis ("x")
#' @param ylab Title of th y-axis ("y")
#'
#' @return 3D figure based on rayshader. \code{overview_print}
#' @examples
#' plot3D()
#' @export
#'

plot3D <- function(
  f = function(x, y) x^0.2*y^0.2, x.min = 0, y.min = 0, x.max = 10, y.max = 10,
  n = 100, low = "blue", high = "red",  title = NULL, legend.title = "f",
  xlab = "x", ylab = "y"
) {

  if(is.null(title)) title <- as.expression(functionBody(f)) # set default to title

rayshader::plot_gg(
ggplot2::ggplot(data = dplyr::mutate(
    tidyr::crossing(x = seq(from = x.min, to = x.max, length.out = n),
                    y = seq(from = x.min, to = x.max, length.out = n)),
    f = f(x, y)
  ), mapping = ggplot2::aes(x = x, y = y, color = f)
) + ggplot2::geom_point() +
  ggplot2::scale_color_gradient(low = low, high = high) +
  ggplot2::labs(title = title, color = legend.title, x = xlab, y = ylab) +
  ggplot2::scale_x_continuous(expand = c(0,0)) +
  ggplot2::scale_y_continuous(expand = c(0,0))
)
}
