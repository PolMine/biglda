#' Instantiate and load mallet topicmodel
#' 
#' @param filename Filename of a mallet topic model (ParallelTopicModel).
#' @details The function \code{mallet_load_topicmodel} will load a topic model
#'   created using mallet into memory.
#' @rdname paralleltopicmodel
#' @export mallet_load_topicmodel
#' @importFrom rJava .jnew .jarray J
#' @examples 
#' pta <- ParallelTopicModel()
#' destfile <- tempfile()
#' pta$write(rJava::.jnew("java/io/File", destfile))
#' pta_reloaded <- mallet_load_topicmodel(destfile)
mallet_load_topicmodel <- function(filename){
  rJava::J("cc/mallet/topics/RTopicModel")$read(rJava::.jnew("java/io/File", filename))
}

#' @param n_topics Number of topics (\code{integer} value).
#' @param alpha_sum Passed into constructor.
#' @param beta Passet into constructor.
#' @details The \code{ParallelTopicModel} function will instantial a Java class
#'   object with the same name from the mallet package, see the
#'   \href{http://mallet.cs.umass.edu/api/cc/mallet/topics/ParallelTopicModel.html}{mallet
#'   documentation} of the class.
#' @rdname paralleltopicmodel
#' @export ParallelTopicModel
ParallelTopicModel <- function(n_topics = 25L, alpha_sum = 5.1, beta = 0.1){
  rJava::.jnew("cc/mallet/topics/RTopicModel", as.numeric(n_topics), alpha_sum, beta)
}


#' @details The \code{BigTopicModel} function will instantiate a Java class
#'   object \code{BigTopicModel} which inherits from the RTopicModel and the
#'   ParallelTopicModel class. It adds a method $getDocLengthCounts() to the
#'   the classes it inherits from to provide a fast access to document 
#'   lengths.
#' @rdname paralleltopicmodel
#' @export BigTopicModel
#' @examples
#' bigmodel <- BigTopicModel()
#' bigmodel$read(
#'   rJava::.jnew(
#'     "java/io/File",
#'     system.file(package = "biglda", "extdata", "mallet", "lda_mallet2.bin")
#'   )
#' )
#' bigmodel$getDocLengthCounts()
BigTopicModel <- function(n_topics = 25L, alpha_sum = 5.1, beta = 0.1){
  rJava::.jnew("BigTopicModel", as.numeric(n_topics), alpha_sum, beta)
}



.mallet_cmd <- function(mallet_bin_dir, sourcefile, destfile, topwords = 50, topics = 50, iterations = 2000, threads = NULL){
  stopifnot(
    is.numeric(topics), topics > 2,
    is.numeric(iterations), iterations > 1,
    is.numeric(topwords), topwords > 2
    #    , is.numeric(threads), threads > 0
  )
  
  cmd <- c(
    file.path(mallet_bin_dir, "mallet"), "train-topics",
    "--input", sourcefile,
    "--num-topics", topics,
    "--num-iterations", iterations,
    "--output-model", destfile,
    if (is.null(threads)) c() else c("--num-threads", threads)
  )
  paste(cmd, collapse = " ")
}



#' Process large topic word weights matrices
#' 
#' The word weights matrix (weights of words for topics) can get big dataish when 
#' there is a large number of topics and a substantially sized vocabulary. The 
#' \code{mallet_save_word_weights} and the \code{mallet_load_word_weights} are 
#' tools to handle this scenario by writing out the data to disk as a sparse matrix, 
#' and loading this into the R session. In order to be able to use the function,
#' the \code{ParallelTopicModel} class needs to be used, the \code{RTopicModel} will
#' not do it.
#' 
#' @param filename A file with word weights.
#' @export mallet_load_word_weights
#' @importFrom slam simple_triplet_matrix
#' @rdname word_weights
#' @examples
#' \dontrun{
#' polmineR::use("polmineR")
#' speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
#' 
#' library(rJava)
#' .jinit()
#' .jaddClassPath("/opt/mallet-2.0.8/class") # after .jinit(), not before
#' .jaddClassPath("/opt/mallet-2.0.8/lib/mallet-deps.jar")
#' 
#' 
#' # This is the call used internally by 'as_LDA()'. The difference
#' # is that the arguments of the $getTopicWords()-method are FALSE 
#' # (argument 'normalized') and TRUE (argument 'smoothed')
#' beta_1 <- rJava::.jevalArray(lda$getTopicWords(FALSE, TRUE), simplify = TRUE) 
#' alphabet <- strsplit(lda$getAlphabet()$toString(), "\n")[[1]]
#' colnames(beta_1) <- alphabet
#' beta_1 <- beta_1[, alphabet[order(alphabet)] ]
#' rownames(beta_1) <- as.character(1:nrow(beta_1))
#' 
#' # This is an approach that uses a (temporary) file written
#' # to disk. The advantage is that it is a sparse matrix that is
#' # passed
#' fname <- mallet_save_word_weights(lda)
#' word_weights <- mallet_load_word_weights(fname)
#' beta_2 <- t(as.matrix(word_weights))
#' 
#' # Demonstrate the equivalence of the two approaches
#' identical(rownames(beta_1), rownames(beta_2))
#' identical(colnames(beta_1), colnames(beta_2))
#' identical(apply(beta_1, 1, order), apply(beta_2, 1, order))
#' identical(beta_1, beta_2)
#' }
mallet_load_word_weights <- function(filename){
  word_weights_raw <- read.table(filename, sep = "\t")
  simple_triplet_matrix(
    i = as.integer(word_weights_raw[,2]),
    j = word_weights_raw[,1] + 1L,
    v = word_weights_raw[,3],
    dimnames = list(
      levels(word_weights_raw[,2]),
      as.character(1L:(max(word_weights_raw[,1]) + 1L))
    )
  )
}

#' @details The function \code{mallet_save_word_weights} will write a file that
#'   can be handled as a sparse matrix to a file (argument \code{destfile}).
#'   Internally, it uses the method \code{printTopicWordWeights} of the
#'   \code{ParallelTopicModel} class. The (parsed) content of the file is
#'   equivalent to matrix that can be obtained directly the class using the
#'   \code{getTopicWords(FALSE, TRUE)} method. Thus, values are not normalised,
#'   but smoothed (= coefficient beta is added to values).
#' @param model A topic model (class \code{jobjRef}).
#' @param destfile Length-one \code{character} vector, the filename of the
#'   output file.
#' @rdname word_weights
#' @export mallet_save_word_weights
mallet_save_word_weights <- function(model, destfile = tempfile()){
  file <- rJava::.jnew("java/io/File", destfile)
  file_writer <- rJava::.jnew("java/io/FileWriter", file)
  print_writer <- rJava::new(rJava::J("java/io/PrintWriter"), file_writer)
  model$printTopicWordWeights(print_writer)
  print_writer$close()
  destfile 
}


#' Get sparse beta matrix
#' 
#' The beta matrix reporting word weights for topics can grow extremely large. The
#' straight-forward ways to get the matrix can be slow and utterly memory inefficient.
#' This function uses the \code{topicXMLReport()}-method of the \code{ParallelTopicModel}
#' that is the most memory efficient solution we now at this stage. The trick is that
#' weights are only reported for the top N words. Thus you can process the data as
#' as sparse matrix, which is the memory efficient solution. See the examples as a proof
#' that the result is equivalent indeed to the \code{getTopicWords()}-method. Note 
#' however that the matrix is neither normalized nor smoothed nor algorithmized.
#' 
#' @param x A \code{ParallelTopicModel} class object
#' @param n_topics A length-one \code{integer} vector, the number of topics.
#' @param destfile Length-one \code{character} vector, the filename of the
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


