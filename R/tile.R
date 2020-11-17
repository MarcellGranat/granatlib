#' @title tile
#'
#' @description Creates a plot based on ggplot2::geom_tile().
#' @param df
#' @param symetric Should print only the right-bottom part? (F)
#' @param fill Should add fill argument? (T)
#' @param reverse.x Should the text on x axis printed reversed? (T)
#' @param grey Print in black&white? (F)
#' @param x X-axis title ("")
#' @param y Y-axis title ("")
#' @param palette Palette color (brewer) ("PRGn")
#' @param hide.legend Should remove the legend title? (T)
#' @param high Color of highest value (Red)
#' @param low Color of lower value (Blue)
#' @param mid Color of lower value ("white")
#' @param midpoint Midpont of scale (0)
#'
#' @return Data.frame, which has 3 column: the compared 2 variables & the value of cramer indicator. \code{overview_print}
#' @examples
#' data(Affairs, package = "AER")
#' x <- cramer(Affairs)
#' tile(x, symetric = T)
#' @export
#'

tile <- function(df, symetric = F, fill = T, reverse.x = T, grey = F, high = NULL, x = "", y = "", palette = "PRGn",
                 hide.legend = T, low = NULL, mid = "white", midpoint = NULL) {
  names(df) <- c("x", "y", "value")
  if (symetric) {
  df <- dplyr::filter(df, y < x)
  }
  p <- ggplot(data = df, aes(x, y, fill = value)) +
    geom_tile(color = "black") + xlab(x) + ylab(y) +
    theme_minimal()
  if (hide.legend) p <- p + theme(legend.title = element_blank())
  if (reverse.x) p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.55))
  if (!is.numeric(df$value) & grey) p + scale_fill_grey() + theme(legend.position = "bottom")
  if (is.numeric(df$value)) {
   if (grey) high = "black"
   if (min(df$value) < 0 & max(df$value) > 0) {
     p + scale_fill_gradient2(low = ifelse(is.null(low), "#00A3AB", low),
                              high = ifelse(is.null(high), "#FF5B6B", low),
                              mid = ifelse(is.null(mid), 0, mid),
                              midpoint = ifelse(is.null(midpoint), 0, midpoint),
                              guide = guide_colourbar(ticks.colour = "black",
                                                      frame.colour = "black"))
   } else{
     p + scale_fill_gradient2(low = ifelse(is.null(low), "white", low),
                              high = ifelse(is.null(high), "#FF5B6B", high),
                              guide = guide_colourbar(ticks.colour = "black",
                                                      frame.colour = "black"))
   }
  } else {
    if (grey) {
      p + scale_fill_grey() + theme(legend.position = "bottom")
    } else {
      p + scale_fill_brewer(palette = palette) + theme(legend.position = "bottom")
    }
  }
}
