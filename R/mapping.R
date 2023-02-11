#' LDA Mallet Class.
#' 
#' @importClassesFrom topicmodels TopicModel LDA LDA_Gibbscontrol
#' @exportClass
setClass("LDA_Mallet", contains = "LDA")


#' @exportClass 
setClass("LDA_Gensim", contains = "LDA")



#' Convert Gensim or Mallet LDA to R class
#' 
#' @param x A Gensim or Mallet topic model (`ParallelTopicModel`).
#' @param verbose A `logical` value, whether to output progress messages.
#' @export as_LDA
#' @rdname as_LDA
#' @importFrom methods is
#' @importFrom cli cli_progress_step cli_progress_done
setGeneric("as_LDA", function(x, ...) standardGeneric("as_LDA"))

#' @details The `as_LDA()`-function will turn an estimated topic model prepared
#'   using 'mallet' into a `LDA_Mallet` object that inherits from classes
#'   defined in the `topicmodels` package. This may be useful for using topic
#'   model evaluation tools available for the `LDA_Gibbs` class, but not for the
#'   immediate output of malled topicmodelling. Note that the gamma matrix is
#'   normalized and smoothed, the beta matrix is the logarithmized matrix of
#'   normalized and smoothed values obtained from the input mallet topic model.
#' @param beta The beta matrix for a topic model.
#' @param gamma The gamma matrix for a topic model.
#' @examples
#' data_dir <- system.file(package = "biglda", "extdata", "mallet")
#' BTM <- mallet_load_topicmodel(
#'   instancefile = file.path(data_dir, "instance_list.mallet"),
#'   statefile = file.path(data_dir, "lda_mallet.gz")
#' )
#' 
#' LDA <- as_LDA(BTM)
#' 
#' # Avoid memory limitations as follows
#' LDA2 <- as_LDA(BTM, beta = matrix(), gamma = matrix())
#' 
#' B(LDA2) <- save_word_weights(BTM, minimized = TRUE) |>
#'   load_word_weights(minimized = TRUE) |>
#'   exp()
#'   
#' G(LDA2) <- save_document_topics(BTM) |>
#'   load_document_topics()
setMethod("as_LDA", "jobjRef",  function(x, verbose = TRUE, beta = NULL, gamma = NULL){
  
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
    "LDA_Mallet",
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
})


#' @param dtm A Document-Term-Matrix (will be turned into BOW data structure).
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
#' @rdname as_LDA
#' @importFrom methods new
setMethod("as_LDA", "gensim.models.ldamodel.LdaModel", function(x, dtm){
  new(
    "LDA_Gensim",
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
})
