first_character_after <- function(y, ch_after, ch_find, replace_with = "} ") {
  location_after <- str_locate_all(y, ch_after)[[1]][,2]
  location_found <- str_locate_all(y, ch_find)[[1]][,2]

  replace_these <- map(location_after, ~ location_found > .) %>%
    map(which) %>%
    map_dbl(1) %>%
    location_found[.]

  ch_location_shift <- (seq_along(replace_these) - 1) * (str_length(replace_with) - str_length(ch_find))

  replace_these <- replace_these + ch_location_shift

  out <- y

  for (i in replace_these) {
    message(out)
    out <- str_c(str_sub(out, end = i - 1), replace_with, str_sub(out, start = i + 1))
  }

  out
}

