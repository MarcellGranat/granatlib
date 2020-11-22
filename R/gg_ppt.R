#' @title gg_ppt
#'
#' @description Copies a gg object into a new powerpoint slide.
#' @param x A gg object (.last.value)
#' @param filename Name of the slide. '.pptx' is redundant here. (plot)
#'
#' @examples
#' ggplot(cars, aes(speed, dist)) + geom_point()
#' gg_ppt()
#' @export
#'

gg_ppt <- function(x = .Last.value, filename = "plot") {
  x <- rvg::dml(ggobj = x)
  doc <- officer::read_pptx()
  doc <- officer::add_slide(doc, layout = "Title and Content", master = "Office Theme")
  doc <- officer::ph_with(doc, x, location = officer::ph_location_fullsize())
  print(doc, target = paste0(filename,".pptx"))
}
