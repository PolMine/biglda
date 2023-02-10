#' Instantiate and load mallet topicmodel
#' 
#' @param verbose A `logical` value, whether to output progress messages.
#' @param binfile Either a `character` vector containing the path of a mallet 
#'   topic model (ParallelTopicModel), tilde expansion will be appied. Or a 
#'   Java file object.
#' @param instancefile Path to a serialized instance list (binary data format).
#' @param statefile Path to a statefile (gzipped text file usually ending with
#'   .gz).
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
#' model <- mallet_load_topicmodel(binfile)
#' 
#' # Restore model from instance- and statefile
#' instancefile <- system.file(
#'   package = "biglda",
#'   "extdata", "mallet", "instance_list.mallet"
#' )
#' statefile <- system.file(
#'   package = "biglda",
#'   "extdata", "mallet", "lda_mallet.gz"
#' )
#' model <- mallet_load_topicmodel(
#'   instancefile = instancefile,
#'   statefile = statefile
#' )
mallet_load_topicmodel <- function(binfile, instancefile, statefile, verbose = TRUE){
  
  if (!mallet_is_installed()) stop("no Mallet installation found!")
  
  stopifnot(is.logical(verbose), length(verbose) == 1L)

  if (!missing(binfile)){
    stopifnot(
      length(binfile) == 1L,
      is(binfile)[1] %in% c("character", "jobjRef")
    )
    
    if (verbose){
      jvm_mem <- J("java/lang/Runtime")$getRuntime()$maxMemory()
      class(jvm_mem) <- "object_size"
      jvm_heap_space <- format(jvm_mem, units = "GB")
      cli::cli_alert_info("JVM heap space: {jvm_heap_space}")
      
      filesize <- get_formatted_filesize(binfile)
      cli::cli_alert_info("file size: {filesize}")
      
      if (filesize > jvm_mem){
        cli_alert_warning("file size exceeds JVM heap space - loading may fail")
      }
    }
    
    if (!is(binfile)[1] == "jobjRef"){
      binfile <- path.expand(binfile)
      if (!file.exists(binfile)) stop(sprintf("file `%s` not found", binfile))
      jfile <- rJava::.jnew("java/io/File", binfile)
    } else {
      jfile <- binfile
    }
    BTM <- rJava::J("BigTopicModel")$read(jfile)
    
  } else {
    if (missing(instancefile))
      stop("If binfile is missing, argument `instancefile` is required.")
    if (missing(statefile))
      stop("If binfile is missing, argument `statefile` is required.")
    
    stopifnot(is.character(instancefile), length(instancefile) == 1L)
    instancefile <- path.expand(instancefile)
    if (!file.exists(instancefile)) stop("`instancefile` does not exist")
    if (verbose){
      filesize <- get_formatted_filesize(instancefile)
      cli::cli_alert_info("file size of `instancefile`: {filesize}")
    }
    
    stopifnot(is.character(statefile), length(statefile) == 1L)
    statefile <- path.expand(statefile)
    if (!file.exists(statefile)) stop("`statefile` does not exist")
    if (verbose){
      filesize <- get_formatted_filesize(statefile)
      cli::cli_alert_info("file size of `statefile`: {filesize}")
    }
    
    if (verbose) cli::cli_progress_step("load instance list")
    instance_j <- rJava::.jnew("java/io/File", instancefile)
    il <- rJava::J("cc/mallet/types/InstanceList")$load(instance_j)
    
    if (verbose) cli::cli_progress_step("load instance list")
    BTM <- BigTopicModel()
    BTM$addInstances(il)
    rm(il)
    gc()
    rJava::J("java/lang/Runtime")$getRuntime()$gc()
    
    if (verbose) cli::cli_progress_step("digest statefile")
    BTM$initializeFromState(rJava::.jnew("java/io/File", statefile))
  }
  
  BTM
}



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


#' Process large Document-Topics-Matrices
#' 
#' @param model A rJava object `ParallelTopicModel` or inheriting from it.
#' @param destfile Path to a file for temporarily saving data on documents and
#'   topics.
#' @param filename Path to file with data exported from Java.
#' @examples
#' m <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin") |>
#'   mallet_load_topicmodel()
#' fname <- save_document_topics(m)
#' y <- load_document_topics(fname)
#' dim(y)
#' @rdname documenttopics
#' @export
save_document_topics <- function(model, destfile = tempfile()){
  file <- rJava::.jnew("java/io/File", path.expand(destfile))
  file_writer <- rJava::.jnew("java/io/FileWriter", file)
  print_writer <- rJava::new(rJava::J("java/io/PrintWriter"), file_writer)
  model$printDenseDocumentTopics(print_writer)
  print_writer$close()
  destfile 
}


#' @param verbose A `logical` value, whether to show progress messages.
#' @importFrom readr read_tsv
#' @importFrom data.table fread melt setnames setDF
#' @rdname documenttopics
#' @export
load_document_topics <- function(filename, verbose = TRUE){
  stopifnot (is.character(filename))
  filename <- path.expand(filename)
  
  filesize <- file.info(filename)$size
  class(filesize) <- "object_size"
  if (verbose) cli_alert_info("Size of data file: {.blue {format(filesize, 'Gb')}}")
  
  data <- fread(filename, sep = "\t")
  docnames <- data[["V2"]]
  data[, "V1" := NULL][, "V2" := NULL]
  setnames(data, old = colnames(data), new = as.character(1L:ncol(data)))
  m <- as.matrix(data)
  rownames(m) <- docnames
  m
}
