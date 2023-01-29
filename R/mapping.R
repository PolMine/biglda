#' @importClassesFrom topicmodels TopicModel LDA LDA_Gibbscontrol
#' @noRd
setClass(
  "LDA_Gibbs",
  representation(
    seedwords = "ANY",
    z = "integer"
  ),
  contains = "LDA",
  prototype(control = new("LDA_Gibbscontrol")
  )
)


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
#' @importFrom methods is
#' @importFrom cli cli_progress_step cli_progress_done
#' @examples
#' data_dir <- system.file(package = "biglda", "extdata", "mallet")
#' statefile <- file.path(data_dir, "lda_mallet.gz")
#' instancefile <- file.path(data_dir, "instance_list.mallet")
#' 
#' il <- rJava::J(
#'   "cc/mallet/types/InstanceList")$load(
#'     rJava::.jnew("java/io/File", instancefile
#' ))
#' 
#' btm <- BigTopicModel()
#' btm$addInstances(il)
#' btm$initializeFromState(rJava::.jnew("java/io/File", statefile))
#' 
#' lda <- as_LDA(btm)
as_LDA <- function(x, verbose = TRUE, beta = NULL, gamma = NULL){
  
  if (!grepl("(RTopicModel|BigTopicModel)", x$getClass()$toString()))
    stop("incoming object needs to be class ParallelTopicModel/RTopicModel/BigTopicModel")
  
  if (verbose) cli_progress_step("get number of documents and number of terms")
  dimensions <- c(
    x$data$size(), # Number of documents
    x$getAlphabet()$size() # Number of terms
  )
  if (verbose) cli_alert_info("number of docs: {.val {dimensions[1]}} / number of terms: {.val {dimensions[2]}}")
  
  if (verbose) cli_progress_step("getting alphabet")
  alphabet <- strsplit(x$getAlphabet()$toString(), "\n")[[1]]
  cli_progress_done()
  if (verbose) cli_alert_info("alphabet length: {.val {length(alphabet)}}")
  
  if (verbose) cli::cli_progress_step("getting document names")
  docs <- x$getDocumentNames()
  cli_progress_done()
  if (verbose) cli_alert_info("number of document names: {.val {length(docs)}}")
  
  if (is.null(gamma)){
    if (verbose) cli_progress_step("getting topic probabilities (gamma matrix)")
    gamma <- rJava::.jevalArray(x$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
  }
  
  if (is.null(beta)){
    if (verbose) cli_progress_step("getting topic word weights (beta matrix)")
    beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
    beta <- log(beta)
  }
  
  if (verbose) cli_progress_step("instantiate LDA_Gibbs class")
  y <- new(
    "LDA_Gibbs",
    Dim = dimensions,
    k = x$getNumTopics(),
    terms = alphabet,
    documents = docs, # Vector containing the document names
    beta = beta, # A matrix; logarithmized parameters of the word distribution for each topic
    gamma = gamma, # matrix, parameters of the posterior topic distribution for each document
    iter = x$numIterations,
    control = new("LDA_Gibbscontrol")
  )
  
  # dirty hack to compensate that the LDA_Gibbs class is not exported
  attr(attr(y, "class"), "package") <- "topicmodels" 
  y
}

