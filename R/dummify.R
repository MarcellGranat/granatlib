#' @title dummify
#'
#' @description Dummify all non-numeric or selected columns (chr vector) of a data.frame
#'
#' @examples
#' dummify(diamonds, "cut")
#' dummify(diamonds)
#' @export
#'

dummify <- function(.data, .col = NULL, sep = "_") {
  if (is.null(.col)) {

    out <- imap_dfc(.data, ~ {
      if (is.numeric(.x)) {
        return(.x)
      } else {
        pos <- unique(as.character(.x))
        map_dfc(pos, function(p){
          tibble(ifelse(.x == p, 1, 0)) %>%
            set_names(str_c(.y, sep, p))
        })
      }
    }
    )
  } else {
    orig_df <- select(.data, - .col)

    dummified_df <- .data %>%
      select(.col) %>%
      imap_dfc(~ {
        pos <- unique(as.character(.x))
        map_dfc(pos, function(p){
          tibble(ifelse(.x == p, 1, 0)) %>%
            set_names(str_c(.y, sep, p))
        })
      }
      )

    out <- bind_cols(orig_df, dummified_df)
  }

  out
}
