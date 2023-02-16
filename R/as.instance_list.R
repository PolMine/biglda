#' Interface to mallet topicmodelling.
#' 
#' Functionality to support the following workflow (see examples): (a) Turn
#' \code{partition_bundle}-object into mallet instance list, (b) store the
#' resulting \code{jobjRef}-object, (c) run mallet topic modelling and (d)
#' turn ParallelTopicModel Java object into \code{LDA_Gibbs} object from
#' package \code{topicmodels}.
#' 
#' @param x A `partition_bundle` object.
#' @param p_attribute Length-one `character` vector, a positional attribute.
#' @param ... further parameters
#' @param verbose A `logical` value, whether to be verbose.
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
#' library(polmineR)
#' use("polmineR")
#' 
#' speeches <- polmineR::as.speeches(
#'   "GERMAPARLMINI", 
#'   s_attribute_name = "speaker", 
#'   s_attribute_date = "date"
#' )
#' 
#' instance_list <- as.instance_list(speeches)
#' lda <- BigTopicModel(25, 5.1, 0.1)
#' lda$addInstances(instance_list)
#' lda$getDocLengthCounts()
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
#' lda2 <- mallet_load_topicmodel(destfile)
#' topicmodels_lda <- as_LDA(lda)
#' @rdname as.instance_list
#' @importFrom polmineR get_token_stream
#' @exportMethod as.instance_list
#' @importFrom rJava J
setGeneric("as.instance_list", function(x, ...) standardGeneric("as.instance_list"))


#' @examples
#' library(polmineR)
#' use("polmineR")
#' speeches <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker", s_attribute_date = "date")
#' speeches_instance_list <- as.instance_list(speeches, p_attribute = "word")
#' 
#' # Pass argument 'subset' to remove stopwords
#' terms_to_drop <- tm::stopwords("de")
#' speeches_instance_list <- as.instance_list(
#'   speeches,
#'   p_attribute = "word",
#'   subset = {!get(p_attribute) %in% bquote(.(terms_to_drop))}
#' )
#' @param ... Arguments passed into `get_token_stream()` call (e.g. argument
#'   `subset` to apply stopwords).
#' @param min_length Minimum length of documents after removing stopwords.
#' @importFrom rJava .jarray
#' @rdname as.instance_list
#' @exportMethod as.instance_list
#' @importFrom polmineR get_corpus
#' @importMethodsFrom polmineR corpus p_attributes
setMethod("as.instance_list", "partition_bundle", function(x, p_attribute = "word", verbose = TRUE, min_length = 1L, ...){
  
  if (verbose) message("... create alphabet")
  alphabet <- rJava::.jnew("cc/mallet/types/Alphabet", rJava::.jnew("java/lang/String")$getClass())
  corpus_obj <- corpus(get_corpus(x))
  vocabulary <- p_attributes(corpus_obj, p_attribute = p_attribute)
  dummy <- alphabet$lookupIndices(.jarray(vocabulary), TRUE)
  
  if (verbose) message("... instantiate pipe")
  pipe <- .jnew("cc/mallet/pipe/TokenSequence2FeatureSequence", alphabet)
  pipe$setTargetAlphabet(alphabet)
  
  if (verbose) message("... decode token stream")
  token_stream_list <- get_token_stream(x, p_attribute = p_attribute, ...)
  
  if (!is.null(min_length)) {
    if (verbose) message("... removing short documents.")
    doc_lengths <- pblapply(token_stream_list, length)
    documents_to_keep <- which(doc_lengths >= min_length)
    
    if (length(documents_to_keep) == 0)
      stop("... all documents are shorter than the minimum length.")
    
    if (verbose){
      message(
        sprintf(
          "... removing %s out of %s documents shorter than %s tokens.", 
          length(token_stream_list) - length(documents_to_keep), 
          length(token_stream_list),
          min_length
        )
      )
    }
    token_stream_list <- token_stream_list[documents_to_keep]
  }
  
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


setOldClass("DocumentTermMatrix")

#' @exportMethod as.instance_list
#' @rdname as.instance_list
#' @examples
#' data("AssociatedPress", package = "topicmodels")
#' il <- as.instance_list(AssociatedPress)
setMethod("as.instance_list", "DocumentTermMatrix", function(x, verbose = TRUE){
  
  stopifnot(is.logical(verbose))
  if (is.null(dimnames(x)[["Terms"]])) stop("dimnames need to define Terms")
  
  if (is.null(dimnames(x)[["Docs"]])){
    docnames <- as.character(1L:nrow(x))
  } else {
    docnames <- dimnames(x)[["Docs"]]
  }

  if (verbose) cli_progress_step("reconstruct individual tokens from matrix")
  j_split <- split(x = x$j - 1L, f = x$i)
  v_split <- split(x = x$v, f = x$i)
  unbagged <- lapply(
    seq_along(j_split),
    function(i) unlist(Map(rep, j_split[[i]], v_split[[i]]), recursive = FALSE)
  )
  if (verbose) cli_progress_done()
  
  as.instance_list(
    x = unbagged,
    vocabulary = dimnames(x)[["Terms"]],
    docnames = docnames
  )
})

#' @param vocabulary A `character` vector with the vocabulary underlying input 
#'   object `x`, in the correct order.
#' @param docnames A `character` vector with document names. Needs to have same
#'   length as input `list` object. If missing, names of the input `list` are 
#'   used as docnames, if present.
#' @param progress A `logical` value, whether to show progress bar.
#' @examples 
#' use("polmineR", corpus = "GERMAPARLMINI")
#' 
#' vocab <- p_attributes("GERMAPARLMINI", p_attribute = "word")
#' 
#' il <- corpus("GERMAPARLMINI") |>
#'   as.speeches(s_attribute_name = "speaker", s_attribute_date = "date") |>
#'   p_attributes(p_attribute = "word", decode = FALSE) |>
#'   as.instance_list(vocabulary = vocab)
#' @rdname as.instance_list
#' @exportMethod as.instance_list
setMethod("as.instance_list", "list", function(x, vocabulary, docnames, verbose = TRUE, progress = TRUE){
  
  stopifnot(
    length(verbose) == 1, is.logical(verbose),
    length(progress) == 1, is.logical(progress)
  )
  
  if (missing(docnames)){
    if (is.null(names(x))){
      stop("Argument `docnames` missing and input list is not named.")
    } else {
      if (verbose) cli_alert_info("argument `docnames`, using names of input `list`.")
      docnames <- names(x)
    }
  }
  
  stopifnot(length(x) == length(docnames))
  
  lexicon <- rJava::.jnew(
    "cc/mallet/types/Alphabet",
    rJava::.jnew("java/lang/String")$getClass()
  )
  lexicon$lookupIndices(vocabulary, TRUE)
  
  # Create a dummy instance with a target.
  
  target <- .jnew(
    "cc.mallet.types.Instance",
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object"),
    .jnew("java.lang.Object")
  )
  target$setData(.jnew("cc/mallet/types/FeatureSequence", lexicon))
  
  instance_list <- rJava::.jnew("cc/mallet/types/InstanceList", lexicon, lexicon)
  .fn <- function(i){
    feature_sequence <- .jnew("cc/mallet/types/FeatureSequence", lexicon, x[[i]])
    instance <- .jnew(
      "cc.mallet.types.Instance",
      .jnew("java.lang.Object"),
      .jnew("java.lang.Object"),
      .jnew("java.lang.Object"),
      .jnew("java.lang.Object")
    )
    instance$setTarget(target)
    instance$setData(feature_sequence)
    instance$setName(.jnew("java/lang/String", docnames[[i]]))
    instance_list$add(instance)
    invisible(NULL)
  }
  if (progress) pblapply(seq_along(x), .fn) else lapply(seq_along(x), .fn)
  
  instance_list
})




#' @param regex A regular expression (length-one `character` vector) used by
#'   Mallet Java code for splitting `character` vector into tokens.
#' @param tolower A `logical` value, whether to lowercase tokens (performed)
#'   by Mallet Java code.
#' @param stopwords Either a path with a plain text file with stopwords (one per
#'   line), or a `character` vector.
#' @examples 
#' instances <- as.speeches("GERMAPARLMINI", s_attribute_name = "speaker", s_attribute_date = "date") %>%
#'   get_token_stream(p_attribute = "word", collapse = " ") %>% 
#'   unlist() %>%
#'   as.instance_list()
#' @rdname as.instance_list
#' @exportMethod as.instance_list
#' @author Andreas Blaette, David Mimno
setMethod("as.instance_list", "character", function(x, regex = "[\\p{L}]+", tolower = FALSE, stopwords = NULL){
  
  # This code is adapted from the 'mallet' package maintained by David Mimno,
  # see: https://github.com/mimno/RMallet/blob/master/mallet/R/mallet.R
  
  ids <- if (is.null(names(x))) as.character(1:length(x)) else names(x)
  
  pipe_list <- rJava::.jnew("java/util/ArrayList")
  
  pipe_list$add(
    rJava::.jnew(
      "cc/mallet/pipe/CharSequence2TokenSequence",
      rJava::J("java/util/regex/Pattern")$compile(regex)
    )
  )
  
  if (tolower)
    pipe_list$add(rJava::.jnew("cc/mallet/pipe/TokenSequenceLowercase"))
  
  if (!is.null(stopwords)){
    
    stopifnot(is.character(stopwords))
    
    if (length(stopwords) == 1L){
      if (file.exists(stopwords)) {
        stopword_file <- fs::path_norm(stopwords)
      }
    } else {
      stopword_file <- tempfile(fileext = ".txt")
      writeLines(text = stopwords, stopword_file)
    }

    pipe_list$add(
      rJava::.jnew(
        "cc/mallet/pipe/TokenSequenceRemoveStopwords",
        rJava::.jnew("java/io/File", stopword_file),
        "UTF-8", FALSE, FALSE, FALSE
      )
    )
    
  }
  
  pipe_list$add(rJava::.jnew("cc/mallet/pipe/TokenSequence2FeatureSequence"))

  pipe <- rJava::.jnew(
    "cc/mallet/pipe/SerialPipes",
    rJava::.jcast(pipe_list, "java/util/Collection")
  )
  
  instance_list <- rJava::.jnew(
    "cc/mallet/types/InstanceList",
    rJava::.jcast(pipe, "cc/mallet/pipe/Pipe")
  )
  rJava::J("cc/mallet/topics/RTopicModel")$addInstances(instance_list, ids, x)
  instance_list
})


#' @rdname as.instance_list
#' @export instance_list_save
instance_list_save <- function(x, filename = tempfile()){
  # This snippet is inspired an unexported function save.mallet.instances in 
  # v1.2.0 of the R mallet package which has not yet been released at CRAN.
  # See: https://github.com/mimno/RMallet/blob/master/mallet/R/mallet.R
  x$save(rJava::.jnew("java/io/File", filename))
  filename
}


#' @details `instance_list_load()` will load a Java `InstanceList` object that has
#'   been saved to disk (e.g. by using the `instance_list_save()` function).
#'   The return value is a `jobjRef` object.
#' @rdname as.instance_list
#' @export instance_list_load
#' @importFrom rJava J
instance_list_load <- function(filename){
  filename <- path.expand(filename)
  if (!file.exists(filename)) stop("file does not exist")
  J("cc.mallet.types.InstanceList")$load(rJava::.jnew("java/io/File", filename))
}
