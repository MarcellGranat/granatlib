tidy_tex <- function(tex_file = NULL, fig_captions = NULL, tab_captions = NULL, note_type = "simple") {
  if (is.null(tex_file)) {
    tex_file <- list.files() %>%
      keep(str_ends, ".tex")

    if (length(tex_file) != 1) {
      stop(crayon::bgRed(".tex file not found or multiple exist in your wd!"))
    } else {

      message("Modify ", tex_file, "? y/n")
      ans = readline();

      if (ans != "y") {
        stop("Stopped.")
      }
    }
  }
  raw_text <- read_lines(tex_file)

  if (is.null(fig_captions)) {
    fig_captions <- list.files(all.files = TRUE, full.names = TRUE, recursive = T) %>%
      keep(str_detect, "fig_captions")
  }

  if (is.null(tab_captions)) {
    tab_captions <- list.files(all.files = TRUE, full.names = TRUE, recursive = T) %>%
      keep(str_detect, "tab_captions")
  }

  captions_df <- fig_captions %>%
    read_lines() %>%
    enframe() %>%
    mutate(
      id = cumsum(str_starts(value, "label:")),
      name = case_when(
        str_starts(value, "label") ~ "label",
        str_starts(value, "caption") ~ "caption",
        str_starts(value, "note") ~ "note",
        TRUE ~ as.character(NA)
      ),
      value = ifelse(!is.na(name), str_remove(value, str_c(name, ": ")), value)
    ) %>%
    fill(name) %>%
    filter(value != "")

  # ref

  latex_labels <- captions_df %>%
    filter(name == "label") %>%
    pull(value) %>%
    unique()



  md_to_latex <- function(x) {

    # simple star
    out <- str_replace_all(x, "\\\\[*]", "TRULY_STAR723651") # impossibly matching chr

    # bold
    for (i in seq(str_count(out, "[*][*]") %/% 2)) {
      out <- str_replace(out, "[*][*]", "\\\\textbf{")
      out <- str_replace(out, "[*][*]", "}")
    }

    # italic
    for (i in seq(str_count(out, "[*]") %/% 2)) {
      out <- str_replace(out, "[*]", "\\\\textit{")
      out <- str_replace(out, "[*]", "}")
    }

    out <- str_replace_all(out, "\n", "\n\n")

    out
  }

  notes_df <- captions_df %>%
    filter(name == "note") %>%
    mutate(value = md_to_latex(value))


  latex_labels <<- latex_labels
  for (i in seq_along(latex_labels)) {

    include_raw <- which(str_detect(raw_text, "includegraphics") & str_detect(raw_text, latex_labels[i]))


    if (length(include_raw) == 1) {

      caption <- captions_df %>%
        filter(id == i, name == "caption") %>%
        pull(value)

      note <- notes_df %>%
        filter(id == i) %>%
        pull(value)

      if (str_starts(raw_text[include_raw - 1], "\\\\begin[{]figure[}]", negate = TRUE)) {
        raw_text <- append(raw_text, "\\end{figure}", include_raw)
        raw_text <- append(raw_text, "\\begin{figure}", include_raw-1)
        include_raw <- include_raw + 1
      }

      if (length(caption) == 1) {
        # raw_text[include_raw] <- gsub("\\caption[{].*", "", raw_text[include_raw])
        raw_text[include_raw] <- gsub("\\\\caption[{].*", "", raw_text[include_raw])
        # if(i == 1) print(raw_text)
        # if(i == 1) print(gsub("\\\\caption[{].*", "", raw_text[include_raw]))


        while (raw_text[include_raw + 1] != "\\end{figure}") {
          raw_text <- raw_text[- (include_raw + 1)] # drop until end
        }
      }

      if (length(note) != 0) {
        note[1] <- str_c("\\floatfoot{", note[1])
        note[length(note)] <- str_c(note[length(note)], "}")

        for (n in rev(note)) {
          raw_text <- append(raw_text, "", include_raw)
          raw_text <- append(raw_text, n, include_raw)
        }
      }
      raw_text <- append(raw_text, str_c("\\label{", latex_labels[i], "}"), include_raw)
      raw_text <- append(raw_text, str_c("\\caption{", caption, "}"), include_raw)

    }
  }

  # tab

  tab_captions_df <- tab_captions %>%
    read_lines() %>%
    enframe() %>%
    mutate(
      id = cumsum(str_starts(value, "label:")),
      name = case_when(
        str_starts(value, "label") ~ "label",
        str_starts(value, "caption") ~ "caption",
        str_starts(value, "note") ~ "note",
        TRUE ~ as.character(NA)
      ),
      value = ifelse(!is.na(name), str_remove(value, str_c(name, ": ")), value)
    ) %>%
    fill(name) %>%
    filter(value != "")

  # ref

  tab_labels <- tab_captions_df %>%
    filter(name == "label") %>%
    pull(value) %>%
    unique()


  #-----

  for (i in c(latex_labels, tab_labels)) {
    raw_text <- str_replace_all(raw_text, str_c("citet[{]", i, "[}]"), str_c("ref{", i, "}"))
  }


  cat(str_flatten(raw_text, "\n"))
  message("\nAll good? y/n")
  ans = readline()
  if (ans == "y") {
    cat(str_flatten(raw_text, "\n"), file = str_c("", tex_file))
    message(crayon::bgGreen(".tex modified!"))
  } else {
    message(crayon::bgRed(".tex untouched!"))
  }


}
