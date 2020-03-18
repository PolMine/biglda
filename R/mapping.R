#' @importClassesFrom topicmodels TopicModel
#' @exportClass LDA_matched
#' @rdname mapping
setClass("LDA_matched", contains = "TopicModel")


#' Convert mallet LDA to topicanalysis class
#' 
#' @param x A mallet topic model (ParallelTopicModel).
#' @param beta The beta matrix for a topic model.
#' @param gamma The gamma matrix for a topic model.
#' @param verbose A \code{logical} value, whether to output progress messages.
#' @details The \code{as_LDA()}-function will turn an estimated topic model
#'   prepared using 'mallet' into a \code{LDA_Gibbs} object as defined in the
#'   \code{topicmodels} package. This may be useful for using topic model
#'   evaluation tools available for the \code{LDA_Gibbs} class, but not for the
#'   immediate output of malled topicmodelling. Note that the gamma matrix is
#'   normalized and smoothed, the beta matrix is the logarithmized matrix of
#'   normalized and smoothed values obtained from the input mallet topic model.
#' @export as_LDA
#' @importFrom pbapply pblapply
#' @rdname mapping
#' @importClassesFrom topicmodels LDA LDA_Gibbscontrol
as_LDA <- function(x, verbose = TRUE, beta = NULL, gamma = NULL){
  
  if (!grepl("RTopicModel", x$getClass()$toString()))
    stop("incoming object needs to be class ParallelTopicModel")
  
  if (verbose) message("... getting number of documents and number of terms")
  dimensions <- c(
    x$data$size(), # Number of documents
    x$getAlphabet()$size() # Number of terms
  )
  
  if (verbose) message("... getting alphabet")
  alphabet <- strsplit(x$getAlphabet()$toString(), "\n")[[1]]
  
  if (verbose) message("... getting document names")
  docs <- pblapply(0L:(x$data$size() - 1L), function(i) x$data$get(i)$instance$getName())
  docs <- x$getDocumentNames()
  
  if (is.null(gamma)){
    if (verbose) message("... getting topic probabilities (gamma matrix)")
    gamma <- rJava::.jevalArray(x$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
  }
  
  if (is.null(beta)){
    message("... getting topic word weights (beta matrix)")
    beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
    beta <- log(beta)
  }
  
  new(
    "LDA_matched",
    Dim = dimensions,
    k = x$getNumTopics(),
    terms = alphabet,
    documents = docs, # Vector containing the document names
    beta = beta, # A matrix; logarithmized parameters of the word distribution for each topic
    gamma = gamma, # matrix, parameters of the posterior topic distribution for each document
    iter = x$numIterations,
    control = new("LDA_Gibbscontrol")
  )
}

