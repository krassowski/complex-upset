% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/upset.R
\name{upset_stripes}
\alias{upset_stripes}
\title{Define appearence of the stripes}
\usage{
upset_stripes(
  mapping = aes(),
  geom = geom_segment(size = 7),
  colors = c("white", "grey95"),
  data = NULL
)
}
\arguments{
\item{mapping}{additional aesthetics}

\item{geom}{a geom to use, should accept \code{x}, \code{y}, \code{xend}, \code{yend} and \code{color} aesthetics}

\item{colors}{a vector of colors to repeat as many times as needed for the fill of stripes, or a named vector specifying colors for values of the variable mapped to the color aesthetics in the mapping argument}

\item{data}{the dataset describing the sets with a column named \code{set} and any other columns as needed for mapping}
}
\description{
Define appearence of the stripes
}
