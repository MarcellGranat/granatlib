replace_references <- function(t, total_labels = c("fig:", "tab:", "eq:")) {

  out <- t

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


  for (p in detected_patterns) {
    message("Replace: ", p)

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

  replace_env_values <- ls()

  if (exists("params")) {
    replace_env_values <- c(replace_env_values, str_c("params$", names(params)))
  }

  for (i in replace_env_values) {
    if (str_detect(out, str_c("@", i))) {
      out <- str_replace_all(out, str_c("@", i), get(i))
    }
  }

  out
}