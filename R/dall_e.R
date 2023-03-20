#' @title Download a Dall-e image.
#'
#' @description This function uses {openai} to generate an image with Dall-e, but downloads it into a specified folder. The snippet for markdown usage also added to the clipboard.
#' @param promt (string) The prompt for the image.
#' @param folder.path (string) The folder where to save the image. Finish it with "/".
#' @param size (string) The size of the generated images. Must be one of 256x256, 512x512, or 1024x1024 (default).
#' @examples
#' dall_e("R programmer writing a new package.")
#' @export
#'

dall_e <- function(promt, folder.path = "figures/", size = "1024x1024") {
  img <- openai::create_image(prompt = prompt, response_format = "url", size = size, return_response = FALSE)

  file_name <- prompt |>
    stringr::str_to_lower() |>
    stringr::str_remove_all("[^A-z ]") |>
    stringr::str_replace_all(" ", "_") |>
    (\(x) paste0(folder.path, x, ".png")) ()

  clipr::write_clip(paste0("![](", file_name, ")"))

  dir.create(folder.path, showWarnings = FALSE)
  download.file(img$data[[1]]$url, destfile = file_name)

  granatlib::info("Image downloaded, snippet copied to your clipboard.", "ok", add_time = FALSE)
}

