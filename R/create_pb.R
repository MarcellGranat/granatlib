#' @title create_pb
#'
#' @description Create progress bar.
#' @examples
#' create_pb(100)
#' @export
#'

create_pb <- function(x, .message = TRUE) {

  if (is.data.frame(x)) {
    n = nrow(x)
    retrive <- TRUE
  } else if (is.numeric(x) & length(x) == 1) {
    n = x
    retrive <- FALSE
  } else {
    n = length(x)
    retrive <- TRUE
  }

  library(progress)

  pb <- progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                              total = n,
                              complete = "=",
                              incomplete = "-",
                              current = ">",
                              force = TRUE,
                              clear = FALSE,
                              width = 100)

  assign("pb", pb, envir = rlang::global_env())
  if (!exists("pb")) assign("pb", pb, envir = rlang::env_parent())

  if (exists("pb") & .message) {
    message(crayon::green("pb created"))
  }

  if (retrive) {
    return(x)
  }
}
