#' @title create_pb
#'
#' @description Create progress bar. If a .data is specified, then create pb with same steps as rows in .data. This is useful for using maps inside dpylr functions. Use n in other cases.
#' @param .data Data frame
#' @param n Number of steps (NULL by default)
#'
#' @return .data \code{overview_print}
#' @examples
#' create_pb()
#' @export
#'

create_pb <- function(.data, n = NULL) {
  if (is.null(n)) {
    if (!is.data.frame(.data)) {
      stop(crayon::bgRed(".data must be a data.frame!"))
    }
  }

  library(progress)
  if (is.null(n)) {
    pb <<- progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                 total = nrow(.data),
                                 complete = "=",
                                 incomplete = "-",
                                 current = ">",
                                 clear = FALSE,
                                 width = 100)
    .data
  } else {
    pb <<- progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                                 total = n,
                                 complete = "=",
                                 incomplete = "-",
                                 current = ">",
                                 clear = FALSE,
                                 width = 100)
  }
}
