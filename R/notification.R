notification <- function(..., sound = FALSE) {

    if (sound) { # any OS
      beepr::beep(1)
    }

  if (stringr::str_detect(osVersion, "macOS")) { # only on MAC
    if (missing(...)) { # if no message specified
      notifier::notify(
        title = gsub(".*/", "", getwd()), # project name as title
        msg = "Work done!" # if no message specified
      )
    } else {
      notifier::notify(
        title = gsub(".*/", "", getwd()),
        msg = ... # if messages specified
      )
    }
  }
}
