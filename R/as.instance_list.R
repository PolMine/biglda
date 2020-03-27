#' Interface to mallet topicmodelling.
#' 
#' Functionality to support the following workflow (see examples): (a) Turn
#' \code{partition_bundle}-object into mallet instance list, (b) store the
#' resulting \code{jobjRef}-object, (c) run mallet topic modelling and (d)
#' turn ParallelTopicModel Java object into \code{LDA_Gibbs} object from
#' package \code{topicmodels}.
#' 
#' @param x A \code{partition_bundle} object.
#' @param corpus A CWB indexed corpus, defined either by corpus ID, or
#'   \code{corpus} object.
#' @param ... further parameters
#' @param p_attribute The p_attribute to use, typically "word" or "lemma".
#' @param verbose A \code{logical} value, whether to be verbose.
#' @param filename Where to store the Java-object.
#' @importFrom utils read.csv read.table
#' @importFrom stats setNames
#' @importFrom slam simple_triplet_matrix
#' @importFrom parallel mclapply
#' @importFrom polmineR get_token_stream
#' @examples  
#' # Preparations: Create instance list
#' 
#' if (!mallet_is_installed()) mallet_install()
#' 
#' polmineR::use("polmineR")
#' speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
#' 
#' instance_list <- as.instance_list(speeches)
#' lda <- ParallelTopicModel(25, 5.1, 0.1)
#' lda$addInstances(instance_list)
#' lda$setNumThreads(1L)
#' lda$setTopicDisplay(50L, 10L)
#' destfile <- tempfile()
#' lda$setSaveSerializedModel(50L, rJava::.jnew("java/lang/String", destfile))
#' lda$setNumIterations(150L)
#' lda$estimate()
#' lda$write(rJava::.jnew("java/io/File", destfile))
#' 
#' # Load topicmodel and turn it into LDA_Gibbs
#' 
#' lda <- mallet_load_topicmodel(destfile)
#' topicmodels_lda <- as_LDA(lda)
#' 
#' @rdname as.instance_list
#' @importFrom polmineR get_token_stream
#' @exportMethod as.instance_list
#' @importFrom rJava J
setGeneric("as.instance_list", function(x, ...) standardGeneric("as.instance_list"))


#' @examples
#' library(polmineR)
#' use("polmineR")
#' speeches <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
#' speeches_instance_list <- as.instance_list(speeches, p_attribute = "word")
#' @importFrom rJava .jarray
#' @rdname as.instance_list
#' @exportMethod as.instance_list
#' @importFrom polmineR get_corpus
#' @importMethodsFrom polmineR corpus p_attributes
setMethod("as.instance_list", "partition_bundle", function(x, p_attribute = "word", verbose = TRUE){
  
  if (verbose) message("... create alphabet")
  alphabet <- rJava::.jnew("cc/mallet/types/Alphabet", rJava::.jnew("java/lang/String")$getClass())
  corpus_obj <- corpus(get_corpus(x))
  vocabulary <- p_attributes(corpus_obj, p_attribute = p_attribute)
  dummy <- alphabet$lookupIndices(.jarray(vocabulary), TRUE)
  
  if (verbose) message("... instantiate pipe")
  pipe <- .jnew("cc/mallet/pipe/TokenSequence2FeatureSequence", alphabet)
  pipe$setTargetAlphabet(alphabet)
  
  if (verbose) message("... decode token stream")
  token_stream_list <- get_token_stream(x, p_attribute = p_attribute)
  
  if (verbose) message("... creating instances")
  instance_list <- rJava::.jnew("cc/mallet/types/InstanceList")
  pblapply(
    names(token_stream_list),
    function(doc_name){
      token_sequence <- .jnew("cc/mallet/types/TokenSequence")
      token_sequence$addAll(.jarray(token_stream_list[[doc_name]]))
      instance <- .jnew(
        "cc.mallet.types.Instance",
        .jnew("java.lang.Object"),
        .jnew("java.lang.Object"),
        .jnew("java.lang.Object"),
        .jnew("java.lang.Object")
      )
      instance$setData(token_sequence)
      instance$setName(rJava::.jnew("java/lang/String", doc_name))
      instance$setTarget(rJava::.jnew("java/lang/String", "foo"))
      instance$setSource(rJava::.jnew("java/lang/String", "foo"))
      instance_list$add(pipe$instanceFrom(instance))
      NULL
    }
  )
  instance_list
})


#' @examples 
#' speeches <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
#' id_list <- p_attributes(speeches, p_attribute = "word", decode = FALSE)
#' instance_list <- as.instance_list(id_list, corpus = "GERMAPARLMINI", p_attribute = "word")
#' @rdname as.instance_list
#' @exportMethod as.instance_list
setMethod("as.instance_list", "list", function(x, corpus, p_attribute = "word"){
  
  lexicon <- rJava::.jnew("cc/mallet/types/Alphabet", rJava::.jnew("java/lang/String")$getClass())
  dummy <- lexicon$lookupIndices(p_attributes(corpus, p_attribute = p_attribute), TRUE)
  
  # Create a dummy instance with a target.
  
  target <- .jnew(
    "cc.mallet.types.Instance",
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object")
  )
  target$setData(.jnew("cc/mallet/types/FeatureSequence", lexicon))
  
  instance <- .jnew(
    "cc.mallet.types.Instance",
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object")
  )
  instance$setTarget(target)
  
  # Create InstanceList.
  
  instance_list <- rJava::.jnew("cc/mallet/types/InstanceList", lexicon, lexicon)
  dummy <- pblapply(
    x,
    function(ids){
      feature_sequence <- .jnew("cc/mallet/types/FeatureSequence", lexicon, ids)
      instance <- .jnew(
        "cc.mallet.types.Instance",
        .jnew("java.lang.Object"),
        .jnew("java.lang.Object"),
        .jnew("java.lang.Object"),
        .jnew("java.lang.Object")
      )
      instance$setTarget(target)
      # if (instance$isLocked()) instance$unLock()
      instance$setData(feature_sequence)
      instance_list$add(instance)
      invisible(NULL)
    }
  )
  instance_list
})

#' @rdname as.instance_list
#' @export mallet_instance_list_store
mallet_instance_list_store <- function(x, filename = tempfile()){
  # This snippet is inspired an unexported function save.mallet.instances in 
  # v1.2.0 of the R mallet package which has not yet been released at CRAN.
  # See: https://github.com/mimno/RMallet/blob/master/mallet/R/mallet.R
  x$save(rJava::.jnew("java/io/File", filename))
  filename
}


#' @details The function \code{mallet_instance_list_load} will load a Java
#'   InstanceList object that has been saved to disk (e.g. by using the
#'   \code{mallet_instance_list_store} function). The return value is a
#'   \code{jobjRef} object. Internally, the function reuses code of the function
#'   \code{load.mallet.instances} from the R package \code{mallet}.
#' @rdname as.instance_list
#' @export mallet_instance_list_load
#' @importFrom rJava J
mallet_instance_list_load <- function(filename){
  J("cc.mallet.types.InstanceList")$load(rJava::.jnew("java/io/File", filename))
}


#' Get topic model diagnostics.
#' 
#' The MALLET topic model toolkit includes a class \code{TopicModelDiagnostics}
#' able to prepare a set of metrics on the topics of a topic model. The function
#' \code{mallet_get_topic_model_diagnostics} will return a \code{data.table}
#' with these diagnostics. See the
#' \href{http://mallet.cs.umass.edu/diagnostics.php}{mallet documentation} for an
#' explanation of the metrics.
#' @param x An instance of the RTopicModel class (java object).
#' @param n Number of the top words that will be evaluated.
#' @examples 
#' \dontrun{
#' dt <- mallet_get_topic_model_diagnostics(lda)
#' }
#' @importFrom data.table := rbindlist
#' @importFrom xml2 read_xml xml_find_all xml_attrs
mallet_get_topic_model_diagnostics <- function(x, n = 100L){
  if (isFALSE(grepl("RTopicModel$", x$getClass()$toString()))){
    stop("Input object is expect to be an instance of the RTopicModel class")
  }
  topic_model_diagnostics <- rJava::.jnew("RTopicModelDiagnostics", x, as.integer(n))
  topic_model_diagnostics_char <- topic_model_diagnostics$toXML()
  topic_model_diagnostics_xml <- xml2::read_xml(topic_model_diagnostics_char)
  nodes <- xml2::xml_find_all(topic_model_diagnostics_xml, xpath = "/model/topic")
  y <- rbindlist(
    lapply(
      xml_attrs(nodes),
      function(x) as.data.table(as.list(setNames(as.numeric(x), names(x))))
    )
  )
  y[, "id" := as.integer(y[["id"]])]
  y
}
