selenium_start <- function(browser = "firefox", port = NULL) {
  library(RSelenium)

  if (is.null(port)) {
    port <- netstat::free_port()
  }

  rs_driver_object <- rsDriver(browser = browser,
                               verbose = FALSE,
                               port = port)

  remDr <<- rs_driver_object$client
}
