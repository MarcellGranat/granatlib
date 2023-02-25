#' @title eu_country_map
#'
#' @description Country map.
#'
#' @examples
#' eurostat::get_eurostat("ei_lmhu_m") |>
#'   group_by(geo) |>
#'   slice_max(time) |>
#'   filter(indic == "LM-UN-M-TOT", s_adj == "SA") |>
#'   select(geo, values) |>
#'   eu_country_map()
#'
#' @export

eu_country_map <- function(x, text_size = 1.5, label_fun = function(x) x) {
  x %>%
    purrr::set_names("geo", "value") %>%
    dplyr::right_join(eurostat::get_eurostat_geospatial(nuts_level = 0), by = "geo") %>%
    dplyr::mutate(
      geo = countrycode::countrycode(geo, "iso2c", "iso3c"),
      s = as.numeric(sf::st_area(geometry))
    ) %>%
    ggplot2::ggplot() +
    ggplot2::aes(geometry = geometry, fill = value) +
    ggplot2::geom_sf(color = "black") +
    ggplot2::geom_sf_text(ggplot2::aes(label = geo, size = s / text_size), color = "white",
                 show.legend = FALSE) +
    ggplot2::scale_x_continuous(limits = c(-25, 45)) +
    ggplot2::scale_y_continuous(limits = c(30, 80)) +
    ggplot2::theme_void() +
    ggplot2::scale_fill_gradient(high = "red4", low = "steelblue", na.value = "white",
                        guide = ggplot2::guide_colorsteps(
                          ticks = T,
                          ticks.colour = "black",
                          frame.colour = "black"
                        ),
                        labels = label_fun)

}
