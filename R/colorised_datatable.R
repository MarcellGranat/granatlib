#' @title colorised_datatable
#'
#' @description Colorise  or draw column for all numeric or selected columns (chr vector) of a data.frame
#'
#' @examples
#' colorised_datatable(iris, "Sepal.Length", type = "bar")
#' colorised_datatable(iris)
#' @export
#'

colorised_datatable <- function(.data, .cols = NULL, type = "bg-color", ...) {
  if (type != "bar" & type != "bg-color") {
    stop("type must be one of bar or bg-color")
  }

  if (is.null(.cols)) {
    color_vars <- .data %>%
      ungroup() %>%
      select_if(is.numeric) %>%
      names()
  } else {
    color_vars <- .data %>%
      ungroup() %>%
      select(.cols) %>%
      names()
  }

  library(DT)
  out <- datatable(.data, ...)
  if (type == "bar") {

    for (i in color_vars) {

      out <- out %>%
        formatStyle(
          i,
          background = styleColorBar(range(pull(.data, i)), 'lightblue'),
          backgroundSize = '100% 90%',
          backgroundRepeat = 'no-repeat',
          backgroundPosition = 'center'
        )
    }
  } else {
    for (i in color_vars) {
      brks <- quantile(pull(.data, i), probs = seq(.05, .95, .05), na.rm = TRUE)
      ramp <- colorRampPalette(c("red", "green"))
      clrs <- ramp(length(brks)+1)

      out <- out %>%
        formatStyle(i, backgroundColor = styleInterval(brks, clrs))
    }
  }

  out
}
