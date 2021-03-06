% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/theme_granat.R
\name{theme_granat}
\alias{theme_granat}
\title{theme_granat}
\usage{
theme_granat(
  base_theme = theme_bw(),
  axis.text = element_text(family = "Times New Roman", size = 12, color = "black"),
  axis.title = element_text(family = "Times New Roman", size = 12, color = "black"),
  text = element_text(family = "Times New Roman", size = 12, color = "black"),
  plot.title = element_text(family = "Times New Roman", size = 12, color = "black",
    face = "bold"),
  plot.tag = element_text(family = "Times New Roman", size = 12, color = "black"),
  plot.subtitle = element_text(family = "Times New Roman", size = 10, color = "black"),
  plot.caption = element_text(family = "Times New Roman", size = 10, color = "black",
    face = "italic"),
  legend.text = element_text(family = "Times New Roman", size = 12, color = "black"),
  legend.title = element_text(family = "Times New Roman", size = 12, color = "black"),
  legend.position = "bottom",
  plot.title.position = "plot",
  plot.tag.position = "topright",
  plot.caption.position = "plot",
  legend.box = "vertical"
)
}
\arguments{
\item{base_theme}{The basic ggplot theme, which will be modified. (theme_bw())}

\item{axis.text}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))}

\item{axis.title}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))}

\item{text}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))}

\item{plot.title}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black", face = "bold"))}

\item{plot.tag}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))}

\item{plot.subtitle}{Gg theme parameter. (element_text(family="Times New Roman", size = 10, color = "black"))}

\item{plot.caption}{Gg theme parameter. (element_text(family="Times New Roman", size = 10, color = "black", face = "italic"))}

\item{legend.text}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))}

\item{legend.title}{Gg theme parameter. (element_text(family="Times New Roman", size = 12, color = "black"))}

\item{legend.position}{Gg theme parameter. ("bottom")}

\item{plot.title.position}{Gg theme parameter. ("plot")}

\item{plot.tag.position}{Gg theme parameter. ("topright")}

\item{plot.caption.position}{Gg theme parameter. ("plot")}

\item{legend.box}{Gg theme parameter. ("vertical")}
}
\value{
GGtheme \code{overview_print}
}
\description{
Ggplot theme formatted to articels require Times New Roman font family.
}
\examples{
theme_set(theme_granat)
}
