#' @title rowid
#'
#' @description Extension of dplyr::row_number(). This generates 001 if there are more than 100 observations.
#' @param prefix Character. Prefix of the generated id. Default is "id".
#' @param suffix Character. Suffix of the generated id. Default is "".
#'
#' @examples
#' tibble::tibble(x = 1:1000) |>
#'  dplyr::mutate(id = rowid())
#' @export
#'

rowid <- function(prefix = "id", suffix = "") {
  rn <- as.character(dplyr::row_number())

  str_c(
    prefix,
    strrep("0", max(stringr::str_length(rn)) - stringr::str_length(rn)),
    rn,
    suffix
  )
}
