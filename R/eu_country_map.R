eu_country_map <- function(x, text_size = 1.5, label_fun = function(x) x) {
  x %>%
    set_names("geo", "value") %>%
    right_join(eurostat::get_eurostat_geospatial(level = 0), by = "geo") %>%
    mutate(
      geo = countrycode::countrycode(geo, "iso2c", "iso3c"),
      s = as.numeric(sf::st_area(geometry))
    ) %>%
    ggplot() +
    aes(geometry = geometry, fill = values) +
    geom_sf(color = "black") +
    geom_sf_text(aes(label = geo, size = s / text_size), color = "white",
                 show.legend = FALSE) +
    scale_x_continuous(limits = c(-25, 45)) +
    scale_y_continuous(limits = c(30, 80)) +
    theme_void() +
    scale_fill_gradient(high = "red4", low = "steelblue", na.value = "white",
                        guide = guide_colorsteps(
                          ticks = T,
                          ticks.colour = "black",
                          frame.colour = "black"
                        ),
                        labels = label_fun)

}
