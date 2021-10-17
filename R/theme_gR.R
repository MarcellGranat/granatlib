theme_gR <- function(base_theme = theme_minimal, family = "Roboto", base_size = 11) {
  eval(parse(text = str_c(base_theme, "(base_family = ",
                          family, ", base_size = ", base_size, ")" ))) +
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
