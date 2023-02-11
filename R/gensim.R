#' Utilities to interface to gensim.
#' 
#' @param model An LDA model trained by gensim.
#' @param dtm A Document-Term-Matrix (will be turned into BOW data structure).
#' @param modeldir Directory where a gensim LDA topic model has been saved.
#' @param modelname Name of a gensim LDA topic model. The data for a model
#'   consists of a set of files starting with the modelname each.
#' @examples
#' if (requireNamespace("reticulate") && reticulate::py_module_available("gensim")){
#'   gensim <- reticulate::import("gensim")
#'   
#'   dir <- system.file(package = "biglda", "extdata", "gensim")
#'   dtmfile <- file.path(dir, "germaparlmini_dtm.rds")
#'   
#'   lda <- gensim_ldamodel_load(modeldir = dir, modelname = "germaparlmini") |>
#'     gensim_ldamodel_as_LDA_Gibbs(dtm = readRDS(dtmfile))
#'     
#'   topics_terms <- topicmodels::get_terms(lda, 10)
#'   docs_topics <- topicmodels::get_topics(lda, 5)
#' }
#' @export gensim_ldamodel_as_LDA_Gibbs
#' @details `gensim_ldamodel_as_LDA_Gibbs()`-function turns a
#'   gensim/Python model (that may have been loaded using
#'   `gensim_ldamodel_load()`) into the class `LDA_Gibbs` well-known
#'   from the `topicmodels` package for further processing within R.
#' @author Andreas Blaette
#' @rdname gensim
#' @importFrom methods new
gensim_ldamodel_as_LDA_Gibbs <- function(model, dtm){
  new(
    "LDA_Mallet",
    Dim = c(
      nrow(dtm), # number of documents
      model$num_terms # number of terms
    ),
    control = new("LDA_Gibbscontrol"),
    k = model$num_topics,
    terms = as.character(model$id2word$id2token),
    documents = rownames(dtm), # Vector containing the document names
    beta = as.matrix(model$expElogbeta), # matrix; logarithmized parameters of the word distribution for each topic
    gamma = model$inference(chunk = dtm_as_bow(dtm), collect_sstats = FALSE)[[1]], # matrix, parameters of the posterior topic distribution for each document
    iter = model$iterations
  )
}

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

#' @details The input to gensim's LDA modelling methods is a representation of
#'   corpora in a data format denoted as "BOW". This utility function
#'   `dtm_as_bow()` turns a sparse matrix (class `simple_triplet_matrix`) into
#'   the bow input format required by gensim.
#' @export dtm_as_bow
#' @author Andreas Blaette
#' @rdname gensim
#' @examples 
#' if (requireNamespace("reticulate") && reticulate::py_module_available("gensim")){
#'   library(polmineR)
#'   gensim <- reticulate::import("gensim")
#'   use("RcppCWB", corpus = "REUTERS")
#' 
#'   bow <- corpus("REUTERS") %>%
#'     split(s_attribute = "id") %>%
#'     as.DocumentTermMatrix(p_attribute = "word", verbose = FALSE) %>%
#'     dtm_as_bow()
#' }   
dtm_as_bow <- function(dtm){
  if (requireNamespace("reticulate")){
    if (!reticulate::py_available(initialize = TRUE)){
      stop("Python not available.")
    } else {
      py <- reticulate::py
    }
    dtm$j <- dtm$j - 1L
    py$j <- reticulate::r_to_py(unname(split(x = dtm$j, f = dtm$i)), convert = TRUE)
    py$v <- reticulate::r_to_py(unname(split(x = dtm$v, f = dtm$i)), convert = TRUE)
    reticulate::py_run_string("corpus = [list(y) for y in [zip(j[x], v[x]) for x in range(len(j))]]")
    return(py$corpus)
  } else {
    stop("Package 'reticulate' required but not available.")
  }
}
