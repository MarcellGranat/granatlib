myplotly <- function(x = NULL, ) {
  plotly::ggplotly(p = x) %>%
    plotly::config(displayModeBar = F)
}
