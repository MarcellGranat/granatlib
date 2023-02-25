#' @title Find publications with dplyr::dplyr::filters based on PorP exported file
#'
#' @description Find publications with dplyr::dplyr::filters based on [PorP](https://harzing.com/resources/publish-or-perish) exported file (also sci-mago dplyr::dplyr::filters)
#' @param file csv file
#' @param n Number of articles
#' @param open Open in your default browser? if FALSE, then data.frame is returned
#' @examples
#' find_publication("https://gist.githubusercontent.com/MarcellGranat/4ed653c8e655d4ebabaa3071fc7b50a0/raw/5aef770bce91b661744ad8c8b1aed56643f795cf/daily-inflation-online.csv", n = 10, FALSE, year > 2015)
#' @export
#'


find_publication <- function(file, n = NULL, open = FALSE, ...) {

if (!exists("scimago")) {

tf <- tempfile(fileext = "csv")
download.file("https://www.scimagojr.com/journalrank.php?out=xls", destfile = tf)

suppressWarnings(
  scimago <- rio::import(tf, format = ";", ) |>
    tibble::tibble()
)

scimago <<- scimago |>
  dplyr::mutate(
    Categories = purrr::map(Categories, str_split_1, ";")
  ) |>
  tidyr::unnest_longer(Categories) |>
  dplyr::mutate(
    Categories_q = stringr::str_extract(Categories, "[()]Q\\d[)]") |>
      readr::parse_number(),
    Categories = stringr::str_remove(Categories, "[()]Q\\d[)]") |>
      stringr::str_trim()
  ) |>
  dplyr::select(Title, SJR, `H index`, Categories, Categories_q, Areas)


}

  out <- read.csv(file) |>
    tibble::tibble() |>
    dplyr::left_join(scimago, c("Publisher" = "Title")) |>
    janitor::clean_names() |>
    dplyr::filter(...) |>
    (\(x) dplyr::slice_max(x, cites, n = ifelse(is.null(n), nrow(x), n))) () |>
    dplyr::distinct(title, .keep_all = TRUE)

  if (open) {
    out |>
      dplyr::filter(!is.na(article_url), !is.null(article_url), article_url != "") |>
      dplyr::pull(article_url) |>
      purrr::walk(browseURL)
  } else {
    out
  }

}
