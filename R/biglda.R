#' biglda-package
#' 
#' Package for fast LDA topic modelling for big corpora.
#' 
#' The \code{ParallelTopicModel} class from the Machine Learning for Language
#' Toolkit 'mallet' offers a fast implementation of an LDA topic model that
#' yields good results. The purpose of the 'biglda' package is to offer a
#' seamless and fast interface to the Java classes of 'mallet' so that the
#' multicore implementation of the LDA algorithm can be used. The \code{as_LDA}
#' function can be used to map the mallet model on the \code{LDA_Gibbs} class
#' from the widely used \code{topicanalysis} package.
#'
#' The mallet Java package is not shipped with the "biglda" package. It needs to
#' be installed explicitly using the \code{mallet_install} function. If mallet
#' is not yet available, a warning will be issued upon loading biglda to install
#' mallet.
#'
#' The R package "mallet" that has been around for a while it the traditional
#' interface to the "mallet" Java package for R users. However, the a
#' \code{RTopicModel} class (cp.
#' \href{https://github.com/mimno/Mallet/blob/af1fcb1f3e6561afac28f4331e4e0d735b3d11b4/src/cc/mallet/topics/RTopicModel.java}{code
#' at GitHub} is used as an interface to the \code{ParallelTopicModel} class,
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
#' \code{ParallelTopicModel} class inaccessible and causing errors that may be
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
BigTopicModel <- function(n_topics = 25L, alpha_sum = 5.1, beta = 0.1){
  rJava::.jnew("BigTopicModel", as.numeric(n_topics), alpha_sum, beta)
}


#' Instantiate and load mallet topicmodel
#' 
#' @param verbose A `logical` value, whether to output progress messages.
#' @param filename Either a `character` vector containing the path of a mallet 
#'   topic model (ParallelTopicModel), tilde expansion will be appied. Or a 
#'   Java file object.
#' @details The function `mallet_load_topicmodel()` will load a topic model
#'   created using mallet into a `BigTopicModel` object.
#' @rdname paralleltopicmodel
#' @export mallet_load_topicmodel
#' @importFrom rJava .jnew .jarray J
#' @importFrom cli cli_alert_info cli_alert_warning
#' @examples 
#' pta <- ParallelTopicModel()
#' destfile <- tempfile()
#' pta$write(rJava::.jnew("java/io/File", destfile))
#' pta_reloaded <- mallet_load_topicmodel(destfile)
#' 
#' binfile <- system.file(
#'   package = "biglda", "extdata", "mallet",
#'   "lda_mallet.bin"
#' )
#' bin <- mallet_load_topicmodel(binfile)
mallet_load_topicmodel <- function(filename, verbose = TRUE){
  
  if (!mallet_is_installed()) stop("no Mallet installation found!")
  
  stopifnot(
    length(filename) == 1L,
    is(filename)[1] %in% c("character", "jobjRef"),
    
    is.logical(verbose),
    length(verbose) == 1L
  )
  
  if (verbose){
    jvm_mem <- J("java/lang/Runtime")$getRuntime()$maxMemory()
    class(jvm_mem) <- "object_size"
    jvm_heap_space <- format(jvm_mem, units = "GB")
    cli::cli_alert_info("JVM heap space: {jvm_heap_space}")
    
    filesize <- file.info(filename)$size
    class(filesize) <- "object_size"
    filesize_human <- format(filesize, "GB")
    cli::cli_alert_info("file size: {filesize_human}")
    
    if (filesize > jvm_mem){
      cli_alert_warning("file size exceeds JVM heap space - loading may fail")
    }
  }
  
  if (!is(filename)[1] == "jobjRef"){
    filename <- path.expand(filename)
    if (!file.exists(filename)) stop(sprintf("file `%s` not found", filename))
    jfile <- rJava::.jnew("java/io/File", filename)
  } else {
    jfile <- filename
  }
  rJava::J("BigTopicModel")$read(jfile)
}

#' @param n_topics Number of topics (single `integer` value).
#' @param alpha_sum Passed into constructor.
#' @param beta Passet into constructor.
#' @details The `ParallelTopicModel()` function will instantial a Java class
#'   object with the same name from the mallet package, see the
#'   \href{http://mallet.cs.umass.edu/api/cc/mallet/topics/ParallelTopicModel.html}{mallet
#'   documentation} of the class.
#' @rdname paralleltopicmodel
#' @export ParallelTopicModel
ParallelTopicModel <- function(n_topics = 25L, alpha_sum = 5.1, beta = 0.1){
  rJava::.jnew("cc/mallet/topics/RTopicModel", as.numeric(n_topics), alpha_sum, beta)
}
