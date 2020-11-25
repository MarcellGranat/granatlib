#' @title theme_granat
#'
#' @description Ggplot theme formatted to articels require Times New Roman font family.
#' @param base_theme The basic ggplot theme, which will be modified. (theme_bw())
#' @param axis.text Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))
#' @param axis.title Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))
#' @param text Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))
#' @param plot.title Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black", face = "bold"))
#' @param plot.tag Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))
#' @param plot.subtitle Gg theme parameter. (element_text(family="Times New Roman", size = 10, color = "black"))
#' @param plot.caption Gg theme parameter. (element_text(family="Times New Roman", size = 10, color = "black", face = "italic"))
#' @param legend.text Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))
#' @param legend.title Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))
#' @param legend.position Gg theme parameter. ("bottom")
#' @param plot.title.position Gg theme parameter. ("plot")
#' @param plot.tag.position Gg theme parameter. ("topright")
#' @param plot.caption.position Gg theme parameter. ("plot")
#' @param legend.box Gg theme parameter. ("vertical")
#' @return GGtheme \code{overview_print}
#' @examples
#' theme_set(theme_granat)
#' @export
#'

theme_granat <- function(base_theme = theme_bw(),
  axis.text = element_text(family="Times New Roman", size = 12, color = "black"),
  axis.title = element_text(family="Times New Roman", size = 12, color = "black"),
  text = element_text(family="Times New Roman", size = 12, color = "black"),
  plot.title = element_text(family="Times New Roman", size = 12, color = "black", face = "bold"),
  plot.tag = element_text(family="Times New Roman", size = 12, color = "black"),
  plot.subtitle = element_text(family="Times New Roman", size = 10, color = "black"),
  plot.caption = element_text(family="Times New Roman", size = 10, color = "black", face = "italic"),
  legend.text = element_text(family="Times New Roman", size = 12, color = "black"),
  legend.title = element_text(family="Times New Roman", size = 12, color = "black"),
  legend.position = "bottom",
  plot.title.position = "plot",
  plot.tag.position = "topright",
  plot.caption.position = "plot",
  legend.box = "vertical") {
  base_theme + theme(
   axis.text = axis.text,
   axis.title = axis.title,
   text = text,
   plot.title = plot.title,
   plot.tag = plot.tag,
   plot.subtitle = plot.subtitle,
   plot.caption = plot.caption,
   legend.text = legend.text,
   legend.title = legend.title,
   legend.position = legend.position,
   plot.title.position = plot.title.position,
   plot.tag.position = plot.tag.position,
   plot.caption.position = plot.caption.position,
   legend.box = legend.box)
}
