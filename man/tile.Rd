% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tile.R
\name{tile}
\alias{tile}
\title{tile}
\usage{
tile(
  df,
  symetric = F,
  fill = T,
  reverse.x = T,
  grey = F,
  high = NULL,
  x = "",
  y = "",
  palette = "PRGn",
  hide.legend = T,
  low = NULL,
  mid = "white",
  midpoint = NULL
)
}
\arguments{
\item{symetric}{Should print only the right-bottom part? (F)}

\item{fill}{Should add fill argument? (T)}

\item{reverse.x}{Should the text on x axis printed reversed? (T)}

\item{grey}{Print in black&white? (F)}

\item{high}{Color of highest value (Red)}

\item{x}{X-axis title ("")}

\item{y}{Y-axis title ("")}

\item{palette}{Palette color (brewer) ("PRGn")}

\item{hide.legend}{Should remove the legend title? (T)}

\item{low}{Color of lower value (Blue)}

\item{mid}{Color of lower value ("white")}

\item{midpoint}{Midpont of scale (0)}
}
\value{
Data.frame, which has 3 column: the compared 2 variables & the value of cramer indicator. \code{overview_print}
}
\description{
Creates a plot based on ggplot2::geom_tile().
}
\examples{
data(Affairs, package = "AER")
x <- cramer(Affairs)
tile(x, symetric = T)
}
