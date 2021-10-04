myplotly <- function(p, tooltip = c("text"), bgcolor = "#2DA2BF") {

    font = list(
      family = "Alright Sans Regular",
      size = 15,
      color = "black"
    )

    label = list(
      bgcolor = bgcolor,
      bordercolor = "transparent",
      font = font
    )

    plotly::ggplotly(p, tooltip = tooltip) %>%
      plotly::style(hoverlabel = label) %>%
      plotly::layout(font = font) %>%
      plotly::config(displayModeBar = FALSE)

}
