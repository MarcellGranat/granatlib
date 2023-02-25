#' @title Create a secret pin
#'
#' @description Create a secret pin
#' @param onedrive_folder Name of the folder to use
#' @export
#'


create_pin <- function(onedrive_folder = NULL) {
  library(pins) # need for usage in your wd
  # create a secret pin names .board

  if (is.null(onedrive_folder)) {
    # name of the project as default
    onedrive_folder <- rstudioapi::getActiveProject() |>
      stringr::str_remove_all(".{0,}/")
  }

  if (exists("od")) {
    .od <- od
  }

  if (!exists(".od")) {
    suppressMessages({
      .od <<- Microsoft365R::get_business_onedrive(tenant = "common")
    })
    # assign od before for using personal account
  } else {
    message("Using the OneDrive account from the env.")
  }

  in_your <- .od$list_files() |>
    dplyr::filter(isdir & name == onedrive_folder) |>
    nrow() |>
    (\(x) x == 1) ()

  if (in_your) {

    tryCatch({
      .board <<- board_ms365(
        drive = .od,
        path = onedrive_folder
      )

      message(crayon::green(".board created"))
    })


  }

  if (!exists(".board")) {

    shared_items <- .od$list_shared_files()

    in_shared <- shared_items |>
      dplyr::filter(isdir & name == onedrive_folder) |>
      nrow() |>
      (\(x) x == 1) ()

    if (in_shared) {
      tryCatch({
        folder_to_board <- shared_items$remoteItem[[which(shared_items$name == onedrive_folder)]]
        .board <<- board_ms365(.od, folder_to_board)

        message(onedrive_folder, ": ", crayon::blue("OneDrive folder is shared w you. "), crayon::green(".board created"))
      })
    }

  }

  if (!exists(".board")) {

    tryCatch({
      .board <<- board_ms365(
        drive = .od,
        path = onedrive_folder
      )

      message(onedrive_folder, ": ", crayon::red("New OneDrive folder is created. "), crayon::green(".board created"))
    })
  }

}
