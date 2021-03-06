% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prtbl.R
\name{prtbl}
\alias{prtbl}
\title{prtbl}
\usage{
prtbl(
  df,
  c = "",
  hun = TRUE,
  digits = 2,
  format.args = NULL,
  align = NULL,
  un = TRUE,
  ufc = F,
  row.names = FALSE,
  escape = TRUE
)
}
\arguments{
\item{c}{Caption to output ("")}

\item{hun}{Logical; Common words should be translated to Hungarian? (TRUE)}

\item{digits}{Number of digits to shown (2)}

\item{format.args}{Decimal mark is set to "," by default if hun is TRUE (NULL)}

\item{align}{(NULL)}

\item{un}{Logical; Names should start with upper case? (TRUE)}

\item{ufc}{Logical; First column should start with upper case? (TRUE)}

\item{row.names}{Logical; Print row names? (FALSE)}

\item{escape}{Logical (TRUE)}
}
\value{
A character vector of the table source code. \code{overview_print}
}
\description{
Print tables based on knitr::kable() function, with
prefered default setup (for Hungarian language)
}
\examples{
df <- broom::tidy(lm(data = cars, formula = speed ~ dist))
prtbl(df, hun = F)
prtbl(df, hun = T)
}
