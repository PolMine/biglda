#' Utilities to interface to gensim.
#' 
#' @param model An LDA model trained by gensim.
#' @param dtm A Document-Term-Matrix (will be turned into BOW data structure).
#' @param modeldir Directory where a gensim LDA topic model has been saved.
#' @param modelname Name of a gensim LDA topic model. The data for a model
#'   consists of a set of files starting with the modelname each.
#' @examples
#' \dontrun{
#' if (requireNamespace("reticulate")){
#'   gensim <- reticulate::import("gensim")
#' 
#'   modeldir <- system.file(package = "biglda", "extdata", "gensim")
#'   modelname <- "germaparlmini"
#'   
#'   dtm <- readRDS(
#'     file = system.file(
#'       package = "biglda", "extdata", "gensim", "germaparlmini_dtm.rds"
#'     )
#'   )
#'   
#'   lda <- gensim_ldamodel_load(
#'     modeldir = system.file(package = "biglda", "extdata", "gensim"),
#'     modelname = "germaparlmini"
#'   )
#' 
#'   y <- gensim_ldamodel_as_LDA_Gibbs(model = lda, dtm = dtm)
#'   topics_terms <- topicmodels::get_terms(y, 10)
#'   docs_topics <- topicmodels::get_topics(y, 5)
#' }
#' }
#' @export gensim_ldamodel_as_LDA_Gibbs
#' @details The \code{gensim_ldamodel_as_LDA_Gibbs()}-function turns a
#'   gensim/Python model (that may have been loaded using
#'   \code{gensim_ldamodel_load}) into the class \code{LDA_Gibbs} well-known
#'   from the \code{topicmodels} package for further processing within R.
#' @author Andreas Blaette
#' @rdname gensim
#' @importFrom methods new
gensim_ldamodel_as_LDA_Gibbs <- function(model, dtm){
  new(
    "LDA_Gibbs",
    Dim = c(
      nrow(dtm), # number of documents
      model$num_terms # number of terms
    ),
    # control = new("TopicModelcontrol"),
    k = model$num_topics,
    terms = as.character(model$id2word$id2token),
    documents = rownames(dtm), # Vector containing the document names
    beta = as.matrix(model$expElogbeta), # matrix; logarithmized parameters of the word distribution for each topic
    gamma = model$inference(chunk = dtm_as_bow(dtm), collect_sstats = FALSE)[[1]], # matrix, parameters of the posterior topic distribution for each document
    iter = model$iterations
  )
}

#' @details Use \code{gensim_ldamodel_load} to load an ldamodel computed by gensim. The 
#' return value is a \code{LdaModel} Python object that can serve as input to functions
#' or that can be processed using the \code{reticulate} package.
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
#'   \code{dtm_as_bow} turns a sparse matrix (class
#'   \code{simple_triplet_matrix}) into the bow input format required by gensim.
#' @export dtm_as_bow
#' @author Andreas Blaette
#' @rdname gensim
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
    reticulate::py_run_string("corpus = [zip(j[x],v[x]) for x in range(len(j))]")
    return(py$corpus)
  } else {
    stop("Package 'reticulate' required but not available.")
  }
}
