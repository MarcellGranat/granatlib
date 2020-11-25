#ggranat <- function(gg, x = "", y = "", color = "", fill = "", grey = T, title = "", subtitle = "",
#                    caption = "", tag = "", x.expand = F, y.expand = F, x.limit = NULL, y.limit = NULL,
#                    is.color = NULL, is.fill = NULL, palette = "PRGn", low = "#e0ecf4", high = "#8856a7") {
#
#
#if(lubridate::is.Date(dplyr::pull(gg$data, as.character(gg$mapping$x[[2]])))) x.expand <- T # is Date?
#if(purrr::is_empty(as.character(gg$mapping$colour[[2]]))) is.color <- T
#if(purrr::is_empty(as.character(gg$mapping$fill[[2]]))) is.fill <- T
#
#  p <- gg + labs(x = x, y = y, color = color, fill = fill, title = title, subtitle = subtitle,
#            caption = caption, tag = tag)
#  if (x.expand) {
#    if (is.null(x.limit)) {
#      p <- p + scale_x_continuous(expand = c(0, 0))
#    } else{
#      p <- p + scale_x_continuous(expand = c(0, 0), limits = x.limit)
#    }
#  }
#
#  if (y.expand) {
#    if (is.null(y.limit)) {
#      p <- p + scale_y_continuous(expand = c(0, 0))
#    } else{
#      p <- p + scale_y_continuous(expand = c(0, 0), limits = x.limit)
#    }
#  }
#
#  if (is.color & is.fill) {
#    if (as.character(gg$mapping$colour[[2]]) == as.character(gg$mapping$fill[[2]])) {
#      if (is.numeric(dplyr::pull(gg$data, as.character(gg$mapping$colour[[2]])))) {
#        if (grey) {
#          p <- p + scale_color_gradient(low = "white", high = "black",
#                guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
#                scale_fill_gradient(low = "white", high = "black")
#        } else {
#          p <- p + scale_color_gradient2(low = low, high = high,
#                guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")) +
#            scale_fill_gradient2(low = low, high = high)
#        }
#      } else {
#        if (grey) {
#          p <- p + scale_color_grey() + scale_fill_grey()
#        } else {
#          p <- p + scale_fill_brewer(palette = palette)
#        }
#      }
#    }
#  }
#}
#
#


#as.character(gg$mapping$colour[[2]])
#as.character(gg$mapping$value[[2]])
#is.date(as.character(gg$mapping$x[[2]]))
#as.character(gg$mapping$y[[2]])
