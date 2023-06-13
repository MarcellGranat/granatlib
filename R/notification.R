#' @title Send a notification on Mac.
#'
#' @description Sends a notification on Mac. Retrives a sound only on Windows.
#' Also send pushover notification that requires to set your app and and user.
#'
#' @param ... Message.
#' @param sound Should play a sound (default FALSE).
#'
#' @export

notification <- function(..., sound = FALSE) {

  if (sound) { # any OS
    beepr::beep(1)
  }

  tryCatch({
    if (missing(...)) { # if no message specified
      pushoverr::pushover(message = "Work done!",
                          user = Sys.getenv("pushover_user"),
                          device = "iphone", app = Sys.getenv("pushover_api"))
    } else {
      pushoverr::pushover(message = ...,
                          user = Sys.getenv("pushover_user"),
                          device = "iphone", app = Sys.getenv("pushover_api"))
    }

  }, error = \(e) message(e))
}
