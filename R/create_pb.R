#' @title Create progress bar.
#'
#' @description Create progress bar.
#' @examples
#' granatlib::create_pb(100)
#' for (i in 1:100) {
#'   Sys.sleep(.1)
#'   pb$tick()
#' }
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

  pb <- progress::progress_bar[["new"]](format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
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
