#' LDA Mallet Class.
#' 
#' @importClassesFrom topicmodels TopicModel LDA LDA_Gibbscontrol
#' @exportClass LDA_Mallet
#' @rdname LDA_classes
#' @slot doclengths An `integer` vector with document lengths.
setClass("LDA_Mallet", slots = c(doclengths = "integer"), contains = "LDA")


#' @rdname LDA_classes
#' @exportClass LDA_Gensim
setClass("LDA_Gensim", contains = "LDA")


#' Convert Gensim or Mallet LDA to R class
#' 
#' @param x A Gensim or Mallet topic model (`ParallelTopicModel`).
#' @param beta A `matrix` with word-topic distribution that will be assigned to
#'   slot 'beta'. If missing (default), the matrix will be derived from the
#'   input model. To assign the matrix in a separate step, use empty matrix
#'   (`matrix()`) as argument.
#' @param gamma A matrix with topic distribution for each document that will be
#'   assigned to slot 'gamma'. If missing (default), the matrix will be derived from the
#'   input model. To assign the matrix in a separate step, use empty matrix
#'   (`matrix()`) as argument.
#' @param verbose A `logical` value, whether to output progress messages.
#' @param ... Further arguments (unused).
#' @export as_LDA
#' @rdname as_LDA
#' @importFrom methods is
#' @importFrom cli cli_progress_step cli_progress_done
as_LDA <- function (x, ...) UseMethod("as_LDA")


#' @details The `as_LDA()`-function will turn an estimated topic model prepared
#'   using 'mallet' into a `LDA_Mallet` object that inherits from classes
#'   defined in the `topicmodels` package. This may be useful for using topic
#'   model evaluation tools available for the `LDA_Gibbs` class, but not for the
#'   immediate output of malled topicmodelling. Note that the gamma matrix is
#'   normalized and smoothed, the beta matrix is the logarithmized matrix of
#'   normalized and smoothed values obtained from the input mallet topic model.
#' @examples
#' data_dir <- system.file(package = "biglda", "extdata", "mallet")
#' BTM <- mallet_load_topicmodel(
#'   instancefile = file.path(data_dir, "instance_list.mallet"),
#'   statefile = file.path(data_dir, "lda_mallet.gz")
#' )
#' 
#' LDA <- as_LDA(BTM)
#' 
#' # Avoid memory limitations by preparing beta/gamma matrix separately
#' LDA2 <- as_LDA(BTM, beta = matrix(), gamma = matrix())
#' 
#' B(LDA2) <- save_word_weights(BTM, minimized = TRUE) |>
#'   load_word_weights(minimized = TRUE) |>
#'   exp()
#'   
#' G(LDA2) <- save_document_topics(BTM) |>
#'   load_document_topics()
#' @rdname as_LDA
#' @exportS3Method 
as_LDA.jobjRef <- function(x, beta, gamma, dtm, verbose = TRUE, ...){
  
  if (!grepl("(RTopicModel|BigTopicModel)", x$getClass()$toString()))
    stop("incoming object needs to be class ParallelTopicModel/RTopicModel/BigTopicModel")
  
  if (length(list(...)) > 0L) warning("Three dots (...) unused in as_LDA.jobjRef() ")
  
  if (verbose) cli_progress_step("instantiate LDA_Gibbs class")
  y <- new(
    "LDA_Mallet",
    k = x$getNumTopics(),
    iter = x$numIterations,
    control = new("LDA_Gibbscontrol")
  )

  if (verbose) cli_progress_step("get number of documents and number of terms")
  y@Dim <- c(
    x$data$size(), # Number of documents
    x$getAlphabet()$size() # Number of terms
  )
  if (verbose) cli_alert_info("number of docs: {.val {y@Dim[1]}} / number of terms: {.val {y@Dim[2]}}")
  
  if (verbose) cli_progress_step("get document lengths")
  y@doclengths <- x$getDocLengthCounts()
  if (verbose) cli_progress_done()
  if (verbose) cli_alert_info("length of doclength vector: {.val {length(y@doclengths)}} (total: {.val {sum(y@doclengths)}}")
  if (length(y@doclengths) != y@Dim[1])
    warning(
      sprintf(
        "number of docs retrieved is %d but length of doclength vector is %d",
        y@Dim[1], length(y@doclengths)
      )
    )
  
  if (verbose) cli_progress_step("getting alphabet")
  y@terms <- strsplit(x$getAlphabet()$toString(), "\n")[[1]]
  cli_progress_done()
  if (verbose) cli_alert_info("alphabet length: {.val {length(y@terms)}}")
  
  if (verbose) cli::cli_progress_step("getting document names")
  y@documents <- x$getDocumentNames()
  cli_progress_done()
  if (verbose) cli_alert_info("number of document names: {.val {length(y@documents)}}")
  
  if (missing(gamma)){
    # A matrix; logarithmized parameters of the word distribution for each topic
    if (verbose) cli_progress_step("getting topic probabilities (gamma matrix)")
    y@gamma <- rJava::.jevalArray(x$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
  }
  
  if (missing(beta)){
    if (verbose) cli_progress_step("getting topic word weights (beta matrix)")
    y@beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
    y@beta <- log(y@beta)
  }

  y
}


#' @param dtm A Document-Term-Matrix (will be turned into BOW data structure).
#'   consists of a set of files starting with the modelname each.
#' @rdname as_LDA
#' @importFrom methods new
#' @exportS3Method 
#' @examples
#' if (requireNamespace("reticulate") && reticulate::py_module_available("gensim")){
#'   gensim <- reticulate::import("gensim")
#'   
#'   dir <- system.file(package = "biglda", "extdata", "gensim")
#'   dtmfile <- file.path(dir, "germaparlmini_dtm.rds")
#'   
#'   lda <- gensim_ldamodel_load(modeldir = dir, modelname = "germaparlmini") |>
#'     as_LDA(dtm = readRDS(dtmfile))
#'     
#'   topics_terms <- topicmodels::get_terms(lda, 10)
#'   docs_topics <- topicmodels::get_topics(lda, 5)
#' }
as_LDA.gensim.models.ldamodel.LdaModel <- function(x, beta, gamma, dtm, verbose = TRUE, ...){
  
  if (length(list(...)) > 0L) warning("Three dots (...) unused in as_LDA.jobjRef() ")
  
  if (verbose) cli_progress_step("Insantiate basic LDA_Gensim S4 class")
  ldamodel <- new(
    "LDA_Gensim",
    Dim = c(
      nrow(dtm), # number of documents
      x$num_terms # number of terms
    ),
    control = new("LDA_Gibbscontrol"),
    k = x$num_topics,
    terms = as.character(),
    documents = rownames(dtm), # Vector containing the document names
    beta = matrix(),
    gamma = matrix(),
    iter = x$iterations
  )
  
  if (verbose) cli_progress_step("assign dictionary (slot 'terms')")
  ldamodel@terms = as.character(x$id2word$id2token)
  
  if (missing(beta)){
    if (verbose)
      cli_progress_step("assign word-topic distribution matrix (slot 'beta')")
    # matrix; logarithmized parameters of the word distribution for each topic
    ldamodel@beta = as.matrix(x$expElogbeta)
  }
  
  if (missing(gamma)){
    if (verbose)
      cli_progress_step("assign topic distribution for each document (slot 'gamma')")
    # matrix, parameters of the posterior topic distribution for each document
    ldamodel@gamma = x$inference(chunk = dtm_as_bow(dtm), collect_sstats = FALSE)[[1]]
  }
  
  ldamodel
}
