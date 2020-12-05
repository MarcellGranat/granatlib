#' @title prtbl
#'
#' @description Print tables based on knitr::kable() function, with
#' prefered default setup (for Hungarian language)
#' @param c Caption to output ("")
#' @param hun Logical; Common words should be translated to Hungarian? (TRUE)
#' @param digits Number of digits to shown (2)
#' @param format.args Decimal mark is set to "," by default if hun is TRUE (NULL)
#' @param align (NULL)
#' @param un Logical; Names should start with upper case? (TRUE)
#' @param ufc Logical; First column should start with upper case? (TRUE)
#' @param row.names Logical; Print row names? (FALSE)
#' @param escape Logical (TRUE)
#'
#' @return A character vector of the table source code. \code{overview_print}
#' @examples
#' df <- broom::tidy(lm(data = cars, formula = speed ~ dist))
#' prtbl(df, hun = F)
#' prtbl(df, hun = T)
#' @export
#'

prtbl <- function(df, c = "", hun = TRUE, digits = 2, format.args = NULL, align = NULL,
                      un = TRUE, ufc = F, row.names = FALSE, escape = TRUE) {
  df <- tibble::as_tibble(df)
  if (is.null(align)) { # Setup default align
    if (is.numeric(df[[1]])) {
      align <- c("c", rep("c", (ncol(df) - 1)))
    } else {
      align <- c("l", rep("c", (ncol(df) - 1)))
    }
  }
  if (is.null(format.args)) {
    if (hun == T) {
      format.args <- list(decimal.mark = ",")
    } else{
      format.args <- list(decimal.mark = ".")
    }
  }
  if ("p.value" %in% names(df)) { # in the case of df is from broom::tidy()
    df[, which(names(df) == "p.value")] <- scales::percent(df[[which(names(df) == "p.value")]], accuracy = .01, decimal.mark = ifelse(hun, ",", "."))
  }
  if (un == T) names(df) <- stringr::str_to_title(names(df))
  if (hun == T) {
    if ("Term" %in% names(df)) df$Term <- ifelse(df$Term == "(Intercept)", "konstans", df$Term)
    if ("term" %in% names(df)) df$term <- ifelse(df$term == "(Intercept)", "konstans", df$term)
    names(df) <- dplyr::case_when(
      names(df) == "Term" ~ "Változó",
      names(df) == "Value" ~ "Érték",
      names(df) == "Variable" ~ "Változó",
      names(df) == "Year" ~ "Év",
      names(df) == "Time" ~ "Idő",
      names(df) == "Estimate" ~ "Koefficiens",
      names(df) == "Std.error" ~ "Standard hiba",
      names(df) == "Statistic" ~ "T-statisztika",
      names(df) == "P.value" ~ "P-érték",
      names(df) == "term" ~ "változó",
      names(df) == "value" ~ "érték",
      names(df) == "variable" ~ "változó",
      names(df) == "year" ~ "év",
      names(df) == "time" ~ "idő",
      names(df) == "estimate" ~ "koefficiens",
      names(df) == "std.error" ~ "standard hiba",
      names(df) == "statistic" ~ "t-statisztika",
      names(df) == "p.value" ~ "p-érték",
      T ~ names(df)
    )
  }
  if (ufc) df[[1]] <- stringr::str_to_title(df[[1]])
  knitr::kable(df,
    caption = c, digits = 2,
    format.args = format.args, align = align, row.names = row.names, escape = escape
  )
}
