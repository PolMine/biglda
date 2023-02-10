#' Access and modify slot for beta matrix.
#' 
#' @param x An object inheriting from the `TopicModel` class.
#' @param value A matrix to assign (beta or gamma).
#' @param ... Further arguments.
#' @exportMethod B
#' @docType methods
#' @rdname TopicModel-methods
setGeneric("B", function(x, ...) standardGeneric("B") )

#' @rdname TopicModel-methods
setMethod("B", "TopicModel", function(x) x@beta) 

#' @exportMethod B<-
#' @rdname TopicModel-methods
setGeneric("B<-", function(x, value) standardGeneric("B<-"))


#' @exportMethod B<-
#' @rdname TopicModel-methods
setReplaceMethod("B", signature = "TopicModel", function(x, value) {
  
  stopifnot(
    is.matrix(value),
    ncol(value) == length(x@terms),
    nrow(value) == x@k
  )
  
  if (is.null(colnames(value)))
    stop("input matrix is required to have colnames")
  if (!all(colnames(value) %in% x@terms))
    stop("not all terms of model present in value")
  
  x@beta <- value[,x@terms]
  dimnames(x@beta) <- NULL
  x
})

#' Process large topic word weights matrices
#' 
#' The word weights matrix (weights of words for topics) can get big dataish
#' when there is a large number of topics and a substantially sized vocabulary.
#' The `save_word_weights()` and the `load_word_weights()` are tools to handle
#' this scenario by writing out the data to disk as a sparse matrix, and loading
#' this into the R session. In order to be able to use the function, the
#' `ParallelTopicModel` class needs to be used, the `RTopicModel` will not do
#' it.
#' @param filename A file with word weights.
#' @param normalized A `logical` value, whether to normalize.
#' @param beta_coeff As a matter of "smoothing", a coefficient is added to the
#'   value oif the matrix. Ideally, state value explicitly in function call. If
#'   missing, it will be guessed from the data.
#' @param verbose A `logical` value, whether to output progress messages.
#' @export load_word_weights
#' @importFrom slam simple_triplet_matrix
#' @rdname word_weights
#' @examples
#' bin <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
#' lda <- mallet_load_topicmodel(bin)
#' fname <- save_word_weights(lda)
#' word_weights <- load_word_weights(fname)
#' @importFrom data.table fread dcast
load_word_weights <- function(filename, minimized = TRUE, beta_coeff, normalized = TRUE, verbose = TRUE){
  if (!file.exists(filename)) stop("file does not exist")
  
  if (verbose) cli_progress_step("read data from disk")
  dt <- data.table::fread(
    file = filename,
    sep = "\t", quote = "", na.strings = "",
    col.names = c("topic", "token", "weight"),
    colClasses = c("integer", "character", "numeric"),
    showProgress = verbose
  )
  if (verbose) cli_progress_done()
  
  if (minimized){
    if (missing(beta_coeff)){
      beta_coeff <- unique(
        round(
          dt[["weight"]] - trunc(dt[["weight"]]),
          digits = 2
        )
      )
      if (verbose) cli_alert_info("guessed beta coefficient: {.val {beta_coeff}}")
    }
    dt_ext <- dcast(
      dt,
      topic ~ token,
      value.var = "weight",
      fill = beta_coeff
    )
    beta <- as.matrix(
      dt_ext[, 2L:ncol(dt_ext)],
      rownames = dt_ext[[1]] + 1
    )
  } else {
    vocabsize <- nrow(dt[dt[["topic"]] == 0])
    vocab <- dt[["token"]][1:vocabsize]
    if (verbose) cli_alert_info("vocabulary size: {.val {vocabsize}}")
    n_topics <- max(dt[["topic"]]) + 1L
    if (verbose) cli_alert_info("number of topics: {.val {n_topics}}")
    
    dt[, "topic" := NULL][, "token" := NULL]
    gc()
    
    if (verbose) cli_progress_step("create matrix")
    beta <- matrix(
      data = dt[["weight"]],
      nrow = n_topics,
      ncol = vocabsize,
      byrow = TRUE,
      dimnames = list(as.character(1L:n_topics), vocab)
    )
  }
  
  if (normalized){
    if (verbose) cli_alert_info("normalize beta matrix")
    # inspired by Mallet code:
    # topicNormalizers[topic] = 1.0 / (tokensPerTopic[topic] + numTypes * beta);
    beta_coeff <- min(beta)
    if (verbose) cli_alert_info("beta coefficient: {.val {beta_coeff}}")
    tokens_per_topic <- apply(beta - beta_coeff, 1L, sum) # undo smoothing
    topic_normalizers <- tokens_per_topic + (ncol(beta) * beta_coeff)
    beta <- beta / topic_normalizers
  }

  beta
}

#' @details The function `save_word_weights()` will write a file that can be
#'   handled as a sparse matrix to a file (argument `destfile`). Internally, it
#'   uses the method `$printTopicWordWeights()` of the `ParallelTopicModel`
#'   class. The (parsed) content of the file is equivalent to matrix that can be
#'   obtained directly the class using the `$getTopicWords(FALSE, TRUE)` method.
#'   Thus, values are not normalised, but smoothed (= coefficient beta is added
#'   to values).
#' @param model A topic model (class `jobjRef`).
#' @param destfile Length-one `character` vector, the filename of the
#'   output file.
#' @param minimized A `logical` value, whether to print word weights with 
#'   nonzero values (without smoothing) only.
#' @rdname word_weights
#' @export save_word_weights
save_word_weights <- function(model, destfile = tempfile(), minimized = FALSE, verbose = TRUE){
  file <- rJava::.jnew("java/io/File", destfile)
  file_writer <- rJava::.jnew("java/io/FileWriter", file)
  print_writer <- rJava::new(rJava::J("java/io/PrintWriter"), file_writer)
  
  if (minimized){
    model$printNonzeroTopicWordWeights(print_writer)
  } else {
    model$printTopicWordWeights(print_writer)
  }

  print_writer$close()
  
  if (verbose){
    fsize <- get_formatted_filesize(destfile)
    cli_alert_info("size of exported file: {.val {fsize}}")
  }

  destfile 
}


#' Get sparse beta matrix
#' 
#' The beta matrix reporting word weights for topics can grow extremely large.
#' The straight-forward ways to get the matrix can be slow and utterly memory
#' inefficient. This function uses the `topicXMLReport()`-method of the
#' `ParallelTopicModel` that is the most memory efficient solution we now at
#' this stage. The trick is that weights are only reported for the top N words.
#' Thus you can process the data as as sparse matrix, which is the memory
#' efficient solution. See the examples as a proof that the result is equivalent
#' indeed to the `getTopicWords()`-method. Note however that the matrix is
#' neither normalized nor smoothed nor algorithmized.
#' 
#' @param x A `ParallelTopicModel` class object
#' @param n_topics A length-one `integer` vector, the number of topics.
#' @param destfile Length-one `character` vector, the filename of the
#'   output file.
#' @export mallet_get_sparse_word_weights_matrix
#' @examples 
#' \dontrun{
#' # x is assumed to be any ParallelTopicModel class object
#' m <- mallet_get_sparse_word_weights_matrix(x)
#' beta_sparse <- as.matrix(m)
#' beta_dense <- rJava::.jevalArray(x$getTopicWords(FALSE, FALSE), simplify = TRUE) 
#' rownames(beta_dense) <- as.character(1:nrow(beta_dense))
#' 
#' 
#' identical(max(beta_sparse[1,]), as.integer(max(beta_dense[1,])))
#' identical(
#'   unname(head(beta_sparse[1,][order(beta_sparse[1,], decreasing = TRUE)], 5)),
#'   as.integer(head(beta_dense[1,][order(beta_dense[1,], decreasing = TRUE)], 5))
#' )
#' .fn <- function(x) as.integer(unname(head(x[order(x, decreasing = TRUE)], 50)))
#' identical(apply(beta_sparse, 1, .fn), apply(beta_dense, 1, .fn))
#' }
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom data.table as.data.table setorderv
mallet_get_sparse_word_weights_matrix <- function(x, n_topics = 50L, destfile = tempfile()){
  if (!requireNamespace("rJava", quietly = TRUE))
    stop("Package 'rJava' required, but not available.")
  
  file <- rJava::.jnew("java/io/File", destfile)
  file_writer <- rJava::.jnew("java/io/FileWriter", file)
  print_writer <- rJava::new(rJava::J("java/io/PrintWriter"), file_writer)
  x$topicXMLReport(print_writer, n_topics)
  print_writer$close()
  
  topic_xml_report <- xml2::read_xml(destfile)
  df <- do.call(
    rbind,
    lapply(
      xml2::xml_find_all(x = topic_xml_report, xpath = "/topicModel/topic"),
      function(topic_node){
        topic_id <- as.integer(xml2::xml_attr(x = topic_node, attr = "id"))
        do.call(
          rbind,
          lapply(
            xml2::xml_find_all(x = topic_node, xpath = "./word"),
            function(word_node){
              data.frame(
                topic_id = topic_id,
                token = xml2::xml_text(x = word_node),
                # rank = as.integer(xml2::xml_attr(x = word_node, attr = "rank")),
                count = as.integer(xml2::xml_attr(x = word_node, attr = "count")),
                stringsAsFactors = FALSE
              )
            }
          )
        )
      }
    )
  )
  
  alphabet <- strsplit(x$getAlphabet()$toString(), "\n")[[1]]
  df[["token_id"]] <- pmatch(df[["token"]], alphabet, duplicates.ok = TRUE)
  dt <- as.data.table(df)
  setorderv(dt, cols = c("topic_id", "token_id"))
  y <- slam::simple_triplet_matrix(
    i = dt[["topic_id"]] + 1L,
    j = dt[["token_id"]],
    v = dt[["count"]],
    nrow = x$getNumTopics(),
    ncol = x$getAlphabet()$size(),
    dimnames = list(as.character(1L:x$getNumTopics()), alphabet)
  )
}
