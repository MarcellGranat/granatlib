replace_references <- function(t, total_labels = c("fig:", "tab:", "eq:")) {

  out <- t # raw test

  total_labels <- c("winner-loser", "ar-lollipop", "rolling-coefs", "est-t0", "market-vs-four", "best-nls", "eq:")

  total_pattern <- total_labels %>%
    {str_c("@", ., collapse = "|")}

  detected_patterns <- str_extract_all(t, total_pattern)[[1]]

  detect_chr_location <- function(.text, pattern) {
    str_locate_all(.text, pattern = pattern)   %>%
      first() %>%
      .[, 1] %>%
      as.numeric()
  }

  replaced_references <- vector()


  for (p in detected_patterns) {
    replaced_references <- append(replaced_references, p)

    pattern_location <- str_locate(out, p) %>%
      .[, 2] %>%
      as.numeric()

    not_chr <- map(c("\\w", ":", "-", "_"), detect_chr_location, .text = out) %>%
      reduce(c) %>%
      setdiff(x = seq(str_length(out)))

    closing_location <- detect(not_chr, ~ .x > pattern_location)

    out <- str_c(str_sub(out, end = closing_location - 1), "}", str_sub(out, start = closing_location))
    out <- str_replace(out, p, str_c("\\\\ref{", str_sub(p, start = 2)))

  }

  # environment ref



  for (i in ls(envir = globalenv())) {

    if (str_detect(out, str_c("@", i, c(" ", "\\W"), collapse = "|"))) {

      replacement <- get(i, envir = globalenv()) %>%
        as.character()

      if (as.character(as.numeric(replacement)) == replacement) {
        replacement <- as.numeric(replacement)

        if (replacement %% 1 == 0) {
          n_digits = 0
        } else {
          n_digits = 4
        }

        if (replacement >= 10000) {
          big.mark = ","
        } else {
          big.mark = ""
        }

        replacement <- format(round(replacement, digits = n_digits), big.mark = big.mark, decimal.mark = ".")

      }
      replaced_references <- append(replaced_references, i)
      out <- str_replace_all(out, str_c("@", i), replacement = replacement)
    }
  }




  if (exists("params", envir = globalenv())) {

    for (i in names(params)) {
      if (str_detect(out, str_c("@", i, c(" ", "\\W"), collapse = "|"))) {
        replaced_references <- append(replaced_references, i)

        replacement <- params[[i]] %>%
          as.character()

        if (!is.na(as.numeric(replacement))) {
          replacement <- as.numeric(replacement)

          if (replacement %% 1 == 0) {
            n_digits = 0
          } else {
            n_digits = 4
          }

          if (replacement >= 10000) {
            big.mark = ","
          } else {
            big.mark = ""
          }

          replacement <- format(round(replacement, digits = n_digits), big.mark = big.mark, decimal.mark = ".")

        }

        out <- str_replace_all(out, str_c("@", i), replacement = replacement)
      }
    }
  }

  message(crayon::green("Replaced: ", replaced_references))

  out
}

