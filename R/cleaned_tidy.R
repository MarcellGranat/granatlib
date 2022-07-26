cleaned_tidy <- function(x, hun = FALSE) {

  if (is.data.frame(x)) {
    if (!("estimate" %in% names(x))) {
      stop("x is not an lm object or a tidied data.frame")
    }
  } else {
    if (class(x) != "lm") {
      x <- broom::tidy(x)
    } else {
      stop("x is not an lm object or a tidied data.frame")
    }
  }


  if (hun) {
    f_decimal_mark <- ","
    term_intercept <- "Konstans"
    f_big_mark <- " "
  } else {
    f_decimal_mark <- "."
    term_intercept <- "Intercept"
    f_big_mark <- ","
  }

  if ("p.value" %in% names(x)) {
    p.stars <- case_when(
      x$p.value < .01 ~ "***",
      x$p.value < .05 ~ "** ",
      x$p.value < .1 ~ "*  ",
      TRUE ~ "   "
    )

    x <- x %>%
      mutate(
        p.value = round(p.value, 4),
        p.value = format(p.value, digits = 4, decimal.mark = f_decimal_mark, big.mark = f_big_mark),
        p.value = as.character(p.value),
        p.value = str_c(p.value, p.stars)
      ) %>%
      rename(`P-value` = `p.value`)
  }

  if ("term" %in% names(x)) {
    x <- x %>%
      mutate(term = str_replace(term, "[(]Intercept[)]", term_intercept))
  }

  x <- rename_all(x, str_replace_all, "[.]", ". ")
  x <- rename_all(x, ~ {str_c(str_to_upper(str_sub(., end = 1)), str_sub(., start = 2))})

  x <- mutate_if(x, is.numeric, round, 4)
  x <- mutate_if(x, is.numeric, format, digits = 4, decimal.mark = f_decimal_mark, big.mark = f_big_mark)
  x <- mutate_if(x, is.numeric, as.character)

  return(x)
}
