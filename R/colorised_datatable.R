#' @title colorised_datatable
#'
#' @description Colorise  or draw column for all numeric or selected columns (chr vector) of a data.frame
#'
#' @param .data A data.frame.
#' @param .cols Columns to colorise (by default all the numrical columns).
#' @param type "bar" or "bg-color" (default).
#' @examples
#' colorised_datatable(iris, "Sepal.Length", type = "bar")
#'
#'
#' colorised_datatable(iris)
#' @export
#'

colorised_datatable <- function(.data, .cols = NULL, type = "bg-color", ...) {

  if (type != "bar" & type != "bg-color") {
    stop("type must be one of bar or bg-color")
  }

  if (is.null(.cols)) {
    color_vars <- .data |>
      dplyr::ungroup() |>
      dplyr::select_if(is.numeric) |>
      names()
  } else {
    color_vars <- .data |>
      dplyr::ungroup() |>
      dplyr::select(.cols) |>
      names()
  }

  out <- DT::datatable(.data, ...)
  if (type == "bar") {

    for (i in color_vars) {

      out <- out |>
        DT::formatStyle(
          i,
          background = DT::styleColorBar(range(dplyr::pull(.data, i)), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }
  } else {
    for (i in color_vars) {
      brks <- stats::quantile(dplyr::pull(.data, i), probs = seq(.05, .95, .05), na.rm = TRUE)
      ramp <- grDevices::colorRampPalette(c("red", "green"))
      clrs <- ramp(length(brks)+1)

      out <- out |>
        DT::formatStyle(i, backgroundColor = DT::styleInterval(brks, clrs))
    }
  }

  out
}
