% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gg_ppt.R
\name{gg_ppt}
\alias{gg_ppt}
\title{gg_ppt}
\usage{
gg_ppt(x = .Last.value, filename = "plot")
}
\arguments{
\item{x}{A gg object (.last.value)}

\item{filename}{Name of the slide. '.pptx' is redundant here. (plot)}
}
\description{
Copies a gg object into a new powerpoint slide.
}
\examples{
ggplot(cars, aes(speed, dist)) + geom_point()
gg_ppt()
}
