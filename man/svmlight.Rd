% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/svmlight.R
\name{svmlight}
\alias{svmlight}
\alias{svmlight,DocumentTermMatrix-method}
\title{Generate svmlight format}
\usage{
svmlight(x, ...)

\S4method{svmlight}{DocumentTermMatrix}(x, filename, verbose = TRUE)
}
\arguments{
\item{x}{Input object, `DocumentTermMatrix` for the time being.}

\item{...}{Additional arguments.}

\item{filename}{Path to a file.}

\item{verbose}{A `logical` value, whether to output progress messages.}
}
\description{
The svmlight data format is an input format used by Mallet.
}
\details{
Note that it is (currently) not possible to generate a (Mallet) `InstanceList`
from svmlight input that can be used for Mallet topic modelling: The Mallet
output from svmlight input is a `FeatureVector`, but Mallet topic modelling
requires a `FeatureSequence` as input.
}
\examples{
library(polmineR)
use("RcppCWB")

svmlight_file <- tempfile()
corpus("REUTERS") \%>\%
  as.DocumentTermMatrix(p_attribute = "word", s_attribute = "id") \%>\%
  svmlight(filename = svmlight_file)
}
