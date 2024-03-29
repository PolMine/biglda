#' Load gensim topic model.
#' 
#' @param modeldir Directory where a gensim LDA topic model has been saved.
#' @param modelname Name of a gensim LDA topic model. The data for a model
#' @details Use `gensim_ldamodel_load()` to load an ldamodel computed by gensim.
#'   The return value is a `LdaModel` Python object that can serve as input to
#'   functions or that can be processed using the `reticulate` package.
#' @author Andreas Blaette
#' @rdname gensim
#' @export gensim_ldamodel_load
gensim_ldamodel_load <- function(modeldir, modelname){
  if (requireNamespace("reticulate", quietly = TRUE)){
    gensim <- reticulate::import("gensim")
    lda <- gensim$models$ldamodel$LdaModel$load(file.path(modeldir, modelname))
    return(lda)
  } else {
    stop("Package 'reticulate' required but not available.")
  }
}

#' Generate Gensim input from R.
#' 
#' @param dtm A `DocumentTermMatrix`.
#' @details The input to gensim's LDA modelling methods is a representation of
#'   corpora in a data format denoted as "BOW". This utility function
#'   `dtm_as_bow()` turns a sparse matrix (class `simple_triplet_matrix`) into
#'   the bow input format required by gensim.
#' @export dtm_as_bow
#' @author Andreas Blaette
#' @rdname gensim_input
#' @examples 
#' if (requireNamespace("reticulate") && reticulate::py_module_available("gensim")){
#'   library(polmineR)
#'   use("RcppCWB", corpus = "REUTERS")
#' 
#'   dtm <- corpus("REUTERS") %>%
#'     split(s_attribute = "id") %>%
#'     as.DocumentTermMatrix(p_attribute = "word", verbose = FALSE)
#'   
#'   bow <- dtm_as_bow(dtm)
#'   dict <- dtm_as_dictionary(dtm)
#' }   
dtm_as_bow <- function(dtm){
  
  if (!requireNamespace("reticulate"))
    stop("Package 'reticulate' required but not available.")

  if (!reticulate::py_available(initialize = TRUE))
    stop("Python not available.")
  
  
  py <- reticulate::py

  py$j <- reticulate::r_to_py(
    unname(split(x = dtm$j - 1L, f = dtm$i)),
    convert = TRUE
  )
  py$v <- reticulate::r_to_py(
    unname(split(x = dtm$v, f = dtm$i)), convert = TRUE
  )
  reticulate::py_run_string(
    "corpus = [list(y) for y in [zip(j[x], v[x]) for x in range(len(j))]]"
  )
  
  py$corpus
}

#' @rdname gensim_input
#' @export dtm_as_dictionary
dtm_as_dictionary <- function(dtm){
  if (!requireNamespace("reticulate", quietly = TRUE))
    stop("pkg 'reticulate' required but not available")
  
  if (!reticulate::py_available(initialize = TRUE))
    stop("Python not available.")
  
  if (!reticulate::py_module_available("gensim"))
    stop("module 'gensim' not available")
  
  gensim <- reticulate::import("gensim")

  py <- reticulate::py

  dim_terms <- dtm$dimnames$Terms
  Encoding(dim_terms) <- "UTF-8"
  py$terms <- dim_terms
  reticulate::py_run_string("token2id = dict(zip(terms, range(len(terms))))")
  py$dictionary <- gensim$corpora$dictionary$Dictionary()
  reticulate::py_run_string("dictionary.__dict__['token2id'] = token2id")
  
  py$dictionary
}

