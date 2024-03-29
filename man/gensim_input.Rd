% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gensim.R
\name{dtm_as_bow}
\alias{dtm_as_bow}
\alias{dtm_as_dictionary}
\title{Generate Gensim input from R.}
\usage{
dtm_as_bow(dtm)

dtm_as_dictionary(dtm)
}
\arguments{
\item{dtm}{A `DocumentTermMatrix`.}
}
\description{
Generate Gensim input from R.
}
\details{
The input to gensim's LDA modelling methods is a representation of
  corpora in a data format denoted as "BOW". This utility function
  `dtm_as_bow()` turns a sparse matrix (class `simple_triplet_matrix`) into
  the bow input format required by gensim.
}
\examples{
if (requireNamespace("reticulate") && reticulate::py_module_available("gensim")){
  library(polmineR)
  use("RcppCWB", corpus = "REUTERS")

  dtm <- corpus("REUTERS") \%>\%
    split(s_attribute = "id") \%>\%
    as.DocumentTermMatrix(p_attribute = "word", verbose = FALSE)
  
  bow <- dtm_as_bow(dtm)
  dict <- dtm_as_dictionary(dtm)
}   
}
\author{
Andreas Blaette
}
