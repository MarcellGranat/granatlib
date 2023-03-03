#' @title geom_tag
#'
#' @description highlight a datapoint on the figure with label.
#'
#' @export
#'

geom_tag <- function(x, y, label = "label", color = "cyan4", shape = 16, nudge_x = -.5, nudge_y = -.5, point_size = 3, label_size = 3) {
  .data <- tibble::tibble(x = x, y = y)

  list(ggplot2::geom_point(data = .data, ggplot2::aes(x = x, y = y), color = color, shape = shape, size = point_size),
       ggrepel::geom_label_repel(data = .data, ggplot2::aes(x = x, y = y, label = label),
                                 color = color,
                                 size = label_size,
                                 nudge_x = nudge_x,
                                 arrow = NULL,
                                 nudge_y = nudge_y)
  )
}
