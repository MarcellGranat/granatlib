#' @title Send a notification on Mac.
#'
#' @description Sends a notification on Mac. Retrives a sound only on Windows.
#' @param ... Message.
#' @param sound Should play a sound (default FALSE).
#'
#' @export

notification <- function(..., sound = FALSE) {

  if ("logo.png" %in% list.files()) {
    img <- "logo.png"
  } else {
    img <- NULL
  }
    if (sound) { # any OS
      beepr::beep(1)
    }

  if (stringr::str_detect(osVersion, "macOS")) { # only on MAC
    if (missing(...)) { # if no message specified
      notifier::notify(
        title = gsub(".*/", "", getwd()), # project name as title
        image = img,
        msg = "Work done!" # if no message specified
      )
    } else {
      notifier::notify(
        title = gsub(".*/", "", getwd()),
        image = img,
        msg = ... # if messages specified
      )
    }
  }
}
