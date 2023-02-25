#' @title dummify
#'
#' @description Dummify all non-numeric or dplyr::selected columns (chr vector) of a data.frame
#'
#' @examples
#' dummify(ggplot2::diamonds, "cut")
#'
#' dummify(ggplot2::diamonds)
#' @export
#'

dummify <- function(.data, .col = NULL, sep = "_") {
  if (is.null(.col)) {

    out <- purrr::imap_dfc(.data, ~ {
      if (is.numeric(.x)) {
        return(.x)
      } else {
        pos <- unique(as.character(.x))
        purrr::map_dfc(pos, function(p){
          tibble::tibble(ifelse(.x == p, 1, 0)) |>
            purrr::set_names(stringr::str_c(.y, sep, p))
        })
      }
    }
    )
  } else {
    orig_df <- dplyr::select(.data, - dplyr::all_of(.col))

    dummified_df <- .data |>
      dplyr::select(dplyr::all_of(.col)) |>
      purrr::imap_dfc(~ {
        pos <- unique(as.character(.x))
        purrr::map_dfc(pos, function(p){
          tibble::tibble(ifelse(.x == p, 1, 0)) |>
            purrr::set_names(stringr::str_c(.y, sep, p))
        })
      }
      )

    out <- dplyr::bind_cols(orig_df, dummified_df)
  }

  out
}
