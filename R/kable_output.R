#' @title kable_output
#'
#' @description IF not render then show the table and copy it to clipboard in latex format. Round & decimal mark also managed
#'
#' @return knitr::kable output \code{overview_print}
#' @examples
#' kable_output(iris)
#' @export
#'


kable_output <- function (.data, align = NULL, round_digits = NULL, plus_sign = FALSE,
                          hun = FALSE, same_digits = FALSE, verticals = FALSE, .keep_lines = c(1, 2, -1), force_large = FALSE, caption = NULL, text_contained = NULL, ...)
{
  x <- dplyr::ungroup(.data)
  numeric_cols <- dplyr::select_if(x, is.numeric) |> names()
  if (!is.null(round_digits)) {
    x <- dplyr::mutate_at(x, dplyr::vars(numeric_cols), round, digits = round_digits) |>
      dplyr::mutate_at(dplyr::vars(numeric_cols), as.character)
  }
  if (plus_sign) {
    x <- dplyr::mutate_all(x, as.character) |> dplyr::mutate_at(dplyr::vars(numeric_cols),
                                                   ~dplyr::if_else(stringr::str_starts(., "-"), ., stringr::str_c("+", .)))
  }
  if (hun) {
    x <- dplyr::mutate_all(x, as.character) |> dplyr::mutate_at(dplyr::vars(numeric_cols),
                                                   stringr::str_replace, "[.]", ",")
  }
  if (same_digits) {
    x <- dplyr::mutate_all(x, as.character) |> dplyr::mutate_at(dplyr::vars(numeric_cols),
                                                   function(values) {
                                                     out <- purrr::map_chr(values, ~stringr::str_c(., rep(0, times = max(stringr::str_length(values)) -
                                                                                            stringr::str_length(.))))
                                                     out
                                                   })
  }
  if (is.null(align)) {
    align <- c("l", rep("c", ncol(x) - 1))
  }
  tryCatch(expr = {

    latex_out <- knitr::kable(x, ..., format = "latex", align = align)
    if (!verticals) {
      latex_out <- latex_out |>
        stringr::str_replace_all("l\\|", "l ") |>
        stringr::str_replace_all("c\\|", "c ")
    }
    if (!is.null(.keep_lines)) {

      n_line <- stringr::str_count(latex_out, "hline")

      for (i in 1:n_line) {
        latex_out <- stringr::str_replace(latex_out, "hline", stringr::str_c("hlin", i))
      }

      .keep_lines <- ifelse(.keep_lines > 0, .keep_lines, n_line + 1 + .keep_lines)

      for (i in .keep_lines) {
        latex_out <- stringr::str_replace(latex_out, stringr::str_c("hlin", i), "hline")
      }

      for (i in n_line:1) {
        latex_out <- stringr::str_remove_all(latex_out, stringr::str_c("\\\\hlin", i))
      }

    }

    clipr::write_clip(latex_out)
    message(crayon::bgGreen("Table copied to clipboard in latex format!"))
  }, error = function(e) {
  })

  if (knitr::is_html_output()) {
    if (nrow(.data) > 10) {

      dt_out <- DT::datatable(x, selection = 'none', caption = caption)

      if (!is.null(round_digits)) {
        numeric_cols <- .data |>
          purrr::map_lgl(is.numeric) |>
          which()

        dt_out <- DT::formatRound(dt_out, numeric_cols, digits = round_digits)

      }

      dt_out
    } else {
      knitr::kable(x, ..., align = align, caption = caption)
    }
  } else if (knitr::is_latex_output()) {

    if (is.null(text_contained)) { # if not given explicitly

      text_contained = params$text_contained

      if (is.null(text_contained)) { # if not specified in the YAML
        text_contained = TRUE
      }

    }

    if (!is.null(knitr::opts_current$get()$label) & !is.null(params$tab_captions) & text_contained) {

      label <- knitr::opts_current$get()$label

      tab_captions_df <- readr::read_lines(params$tab_captions) |>
        tibble::enframe() |>
        dplyr::mutate(
          name = dplyr::case_when(
            stringr::str_starts(value, "label") ~ "label",
            stringr::str_starts(value, "caption") ~ "caption",
            stringr::str_starts(value, "note") ~ "note",
            TRUE ~ as.character(NA)
          ),
          value = ifelse(!is.na(name), stringr::str_remove(value, stringr::str_c(name, ": ")), value),
          id = ifelse(name == "label", value, NA),
        ) |>
        tidyr::fill(name) |>
        tidyr::fill(id) |>
        dplyr::filter(value != "", id == label)

      md_to_latex <- function(x) {

        # simple star
        out <- stringr::str_replace_all(x, "\\\\[*]", "TRULY_STAR723651") # impossibly matching chr

        # bold
        for (i in seq(stringr::str_count(out, "[*][*]") %/% 2)) {
          out <- stringr::str_replace(out, "[*][*]", "\\\\\\\\textbf{")
          out <- stringr::str_replace(out, "[*][*]", "}")
        }

        # italic
        for (i in seq(stringr::str_count(out, "[*]") %/% 2)) {
          out <- stringr::str_replace(out, "[*]", "\\\\\\\\textit{")
          out <- stringr::str_replace(out, "[*]", "}")
        }

        out <- stringr::str_replace_all(out, "\n", "\n\n")

        out
      }

      if (is.null(caption)) {
        caption <- tab_captions_df |>
          dplyr::filter(name == "caption") |>
          dplyr::pull(value)
      }

      if (length(caption) == 0) {
        caption <- "..."
      }

      out <- stringr::str_c("\\\\begin{table}\n
  \\\\caption{", caption, "}\n
  \\\\label{", label, "}\n")

      note <- tab_captions_df |>
        dplyr::filter(name == "note") |>
        dplyr::pull(value)

      if (length(note) != 0) {
        note <- note |>
          md_to_latex() |>
          stringr::str_flatten("\n\n")

        out <- out |>
          stringr::str_c("\\\\floatfoot{", note, "}")
      }

      out <- stringr::str_c(out, "\n\n\\\\centering\n\n\\\\begin{tabular}")
      latex_out <- stringr::str_replace(latex_out, pattern = "\\\\begin[{]tabular[}]", out) |>
        stringr::str_c("\\end{table}")
    }

    knitr::asis_output(latex_out)
  } else if (knitr::pandoc_to("docx")) {
    knitr::kable(x, ..., align = align, caption = caption)
  } else if (knitr::pandoc_to("gfm-yaml_metadata_block")) {
    knitr::kable(x, ..., align = align, caption = caption) # github output
  } else {
    .data
  }
}
