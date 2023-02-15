find_publication <- function(file, n = NULL, open = FALSE, ...) {

if (!exists("scimago")) {

tf <- tempfile(fileext = "csv")
download.file("https://www.scimagojr.com/journalrank.php?out=xls", destfile = tf)

suppressWarnings(
  scimago <- rio::import(tf, format = ";", ) |>
    tibble()
)

scimago <<- scimago |>
  mutate(
    Categories = map(Categories, str_split_1, ";")
  ) |>
  unnest_longer(Categories) |>
  mutate(
    Categories_q = str_extract(Categories, "[()]Q\\d[)]") |>
      parse_number(),
    Categories = str_remove(Categories, "[()]Q\\d[)]") |>
      str_trim()
  ) |>
  select(Title, SJR, `H index`, Categories, Categories_q, Areas)


}

  out <- read.csv(file) |>
    tibble() |>
    left_join(scimago, c("Publisher" = "Title")) |>
    janitor::clean_names() |>
    filter(...) |>
    (\(x) slice_max(x, cites, n = ifelse(is.null(n), nrow(x), n))) () |>
    distinct(title, .keep_all = TRUE)

  if (open) {
    out |>
      filter(!is.na(article_url), !is.null(article_url), article_url != "") |>
      pull(article_url) |>
      walk(browseURL)
  } else {
    out
  }

}

