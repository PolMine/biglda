#' biglda-package
#' 
#' Package for fast LDA topic modelling for big corpora.
#' 
#' The `ParallelTopicModel` class from the Machine Learning for Language
#' Toolkit 'mallet' offers a fast implementation of an LDA topic model that
#' yields good results. The purpose of the 'biglda' package is to offer a
#' seamless and fast interface to the Java classes of 'mallet' so that the
#' multicore implementation of the LDA algorithm can be used. The `as_LDA()`
#' function can be used to map the mallet model on the `LDA_Gibbs` class
#' from the widely used `topicanalysis` package.
#'
#' The mallet Java package is not shipped with the "biglda" package. It needs to
#' be installed explicitly using the `mallet_install` function. If mallet
#' is not yet available, a warning will be issued upon loading biglda to install
#' mallet.
#'
#' The R package "mallet" that has been around for a while it the traditional
#' interface to the "mallet" Java package for R users. However, the a
#' `RTopicModel` class (cp.
#' \href{https://github.com/mimno/Mallet/blob/af1fcb1f3e6561afac28f4331e4e0d735b3d11b4/src/cc/mallet/topics/RTopicModel.java}{code
#' at GitHub} is used as an interface to the `ParallelTopicModel` class,
#' hiding the method to define the number of cores to be used from the R used,
#' thus limiting the potential to speed up the computation of a topic model
#' using multiple threads. What is more, functionality for full access to the
#' computed model is hidden, inhibiting the extraction of the information the
#' mallet LDA topic model that is required to map the topic model on the
#' LDA_Gibbs topic model.
#'
#' Note that it is not possible to use the R packages "biglda" and "mallet" in
#' parallel. If "mallet" is loaded, it will put its Java Archive on the
#' classpath of the Java Virtual Machine (JVM), making the latest version of the
#' `ParallelTopicModel` class inaccessible and causing errors that may be
#' difficult to understand. Therefore, a warning will be issued when "biglda"
#' may detect that the JAR included in the "mallet" package is in the classpath.
#' 
#' @examples
#' if (!mallet_is_installed()) mallet_install()
#' @author Andreas Blaette (andreas.blaette@@uni-due.de)
#' @keywords package
#' @docType package
#' @aliases biglda biglda-package
#' @name biglda-package
#' @rdname biglda-package
#' @useDynLib biglda, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
NULL


#' @details The `BigTopicModel` function will instantiate a Java class object
#'   `BigTopicModel` which inherits from the `RTopicModel` and the
#'   `ParallelTopicModel` class. It adds a method `$getDocLengthCounts()` to the
#'   the classes it inherits from to provide a fast access to document lengths.
#' @rdname paralleltopicmodel
#' @export BigTopicModel
#' @examples
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
#' bigmodel <- mallet_load_topicmodel(fname)
#' bigmodel$getDocLengthCounts()
BigTopicModel <- function(instances = NULL, n_topics = 25L, alpha_sum = 5.1, beta = 0.1, threads = 1L, iterations = 1000L, verbose = TRUE, silent = FALSE){

  stopifnot(
    is.numeric(n_topics), length(n_topics) == 1L,
    is.numeric(alpha_sum), length(alpha_sum) == 1L,
    is.numeric(beta), length(beta) == 1L,
    is.numeric(threads), length(threads) == 1L,
    is.numeric(iterations), length(iterations) == 1L,
    is.logical(silent), length(silent) == 1L
  )

  y <- rJava::.jnew("BigTopicModel", as.numeric(n_topics), alpha_sum, beta)
  
  if (!is.null(instances)){
    if (verbose) cli_progress_step("add instances to `BigTopicModel` object")
    y$addInstances(instances)
    if (verbose) cli_progress_done()
  }
  
  y$setNumThreads(as.integer(threads))
  y$setNumIterations(as.integer(iterations))

  if (silent){
    y$setTopicDisplay(0L, 0L) # no intermediate report on topics
    y$logger$setLevel(rJava::J("java.util.logging.Level")$OFF) # remain silent
  }

  y
}


#' @param instances A Mallet `InstanceList` object.
#' @param n_topics Number of topics (single `integer` value).
#' @param alpha_sum Passed into constructor.
#' @param beta Passet into constructor.
#' @param iterations Number of interations to run.
#' @param threads Number of threads/cores to use.
#' @param verbose A `logical` value, whether to show progress messages.
#' @param silent Defaults to `FALSE`, if `TRUE`, all Mallet progress messages
#'   are muted.
#' @details The `ParallelTopicModel()` function will instantial a Java class
#'   object with the same name from the mallet package, see the
#'   \href{http://mallet.cs.umass.edu/api/cc/mallet/topics/ParallelTopicModel.html}{mallet
#'   documentation} of the class.
#' @rdname paralleltopicmodel
#' @export ParallelTopicModel
ParallelTopicModel <- function(n_topics = 25L, alpha_sum = 5.1, beta = 0.1){
  rJava::.jnew("cc/mallet/topics/RTopicModel", as.numeric(n_topics), alpha_sum, beta)
}
