clean_bib <- function(file_name = NULL, clean_journal = TRUE) {
  if (is.null(file_name)) {
    file_name <- list.files() %>%
      keep(str_ends, ".bib") %>%
      enframe() %>%
      arrange(value != "references.bib") %>%
      pull(value) %>%
      first()

    message("Modify ", file_name, "? y/n")
    ans = readline();

    if (ans != "y") {
      stop("Stopped.")
    }
  }

  reference_items <- read_lines(file_name) %>%
    {str_c(., "ffff")} %>%
    {ifelse(. == "}ffff", "ggg", .)} %>%
    {ifelse(. == "ffff", "nnnn", .)} %>%
    str_flatten() %>%
    str_split("gggnnnn") %>%
    .[[1]] %>%
    str_split("ffff") %>%
    map(function(x) discard(x, ~ . %in% c("ggg", "ffff", ""))) %>%
    discard(~ length(.) == 0)

  author <- map(reference_items, keep, ~ str_starts(., "\tauthor|\teditor"))

  athor_found <- map_lgl(author, ~ length(.) != 0)

  get_author <- function(x) {
    out <- str_remove(x[1], "\tauthor =|\teditor =") %>%
      str_remove_all("[{]") %>%
      gsub(pattern = "}.*", replacement = "") %>%
      gsub(pattern = ",.*", replacement = "") %>%
      gsub(pattern = "\\w .*", replacement = "") %>%
      str_trim()
  }

  safely_get_author <- safely(get_author, NA, T)

  author <- map(author, get_author) %>%
    map_chr(1)

  get_year <- function(x) {
    c(
      keep(x, str_starts, "\tyear"),
      keep(x, str_starts, "\tdate"),
      ""
    ) %>%
      str_extract(str_flatten(1900:2022)) %>%
      na.omit() %>%
      first() %>%
      str_sub(start = 3)
  }

  year <- map_chr(reference_items, get_year)

  n_author <- enframe(author, name = NULL, value = "author") %>%
    count(author)

  new_id <- data.frame(author, year) %>%
    left_join(n_author, by = "author") %>%
    mutate(author = str_to_lower(author)) %>%
    mutate(id = ifelse(n > 1, str_c(author, year), author))

  # TODO duplicated ID : same author + same year

  new_id <- new_id %>%
    pull(id)

  reference_items <- pmap(list(athor_found, reference_items, new_id), ~{
    if (..1) {
      out <- ..2

      out[1] <- gsub("[{].*", str_c("{", ..3, ","), out[1])

      if (out[1] != ..2[1]) {
        out[1] <- str_c(out[1], " % modified")
      }

    }
    out
  })


  if (clean_journal) {

    journal <- map(reference_items, keep, ~ str_starts(., "\tjournal"))

    journal_exists <- map_lgl(journal, ~ length(.) != 0)

    journal <- journal %>%
      gsub(pattern = ".*[{]", replacement = "") %>%
      gsub(pattern = "[}].*", replacement = "")

    journal <- map_chr(journal, ~ {
      if (str_starts(., "The |the ")) {
        str_remove(., "The |the ")
      } else {
        .
      }
    })

    capitalize_exceptions <-  c('and', 'as', 'as if', 'as long as', 'at', 'but', 'by', 'even if',
                                'for', 'from', 'if', 'if only', 'in', 'into', 'like', 'near',
                                'now that', 'nor', 'of', 'off', 'on', 'on top of', 'once', 'onto',
                                'or', 'out of', 'over', 'past', 'so', 'so that', 'than', 'that', 'the',
                                'till', 'to', 'up', 'upon', 'with', 'when', 'yet')

    clever_capitalize <- function(x) {
      if (x %in% capitalize_exceptions | str_to_upper(x) %in% capitalize_exceptions) {
        x
      } else if (str_to_upper(x) == x) {
        x
      } else {
        str_to_title(x)
      }
    }

    clean_journal <- journal %>%
      map(~ str_split(., " ")[[1]]) %>%
      map(function(w) map(w, clever_capitalize)) %>%
      map(reduce, c) %>%
      map_chr(str_flatten, " ")



    reference_items <- pmap(list(journal_exists, reference_items, clean_journal), ~ {
      out <- ..2
      if (..1) {
        journal_location <- ..2 %>%
          str_starts("\tjournal") %>%
          which()

        out[journal_location] <- str_c("\tjournal = {", ..3, "},", " % modified")
      }

      out
    })

  }

  out <- reference_items %>%
    map(str_flatten, "\n") %>%
    map(str_c, "\n}\n\n") %>%
    reduce(c) %>%
    str_flatten()

  cat(out)

  message("All good? y/n")
  ans = readline()

  if (ans == "y") {
    out <- str_remove_all(out, " % modified")
    cat(out, file = str_c(file_name))
    message(crayon::bgGreen(".bib modified!"))
  } else {
    message(crayon::bgRed(".bib untouched!"))
  }


}
