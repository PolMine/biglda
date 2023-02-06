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
#' speeches <- polmineR::as.speeches(
#'   "GERMAPARLMINI",
#'   s_attribute_name = "speaker",
#'   s_attribute_date = "date"
#' )
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
#' 
#' y2 <- rJava::.jevalArray(m$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
#' dimnames(y2) <- list(m$getDocumentNames(), as.character(1L:ncol(y2)))
#' 
#' testthat::expect_identical(dim(y), dim(y2))
#' y2["Wolfgang Wieland_2009-11-11_1",]
#' as.matrix(y)["Wolfgang Wieland_2009-11-11_1",]
#' testthat::expect_equal(as.matrix(y), y2)
#' @rdname documenttopics
#' @export
save_document_topics <- function(model, destfile = tempfile()){
  jfile <- rJava::.jnew("java/io/File", destfile)
  model$printDocumentTopics(jfile)
  destfile 
}


#' @importFrom readr read_tsv
#' @importFrom data.table fread melt setnames setDF
#' @rdname documenttopics
#' @export
load_document_topics <- function(filename){
  
  stopifnot (is.character(filename))
  filename <- path.expand(filename)
  
  first_data_line <- readLines(filename, n = 2L)[2]
  n_topics <- length(strsplit(first_data_line, "\t")[[1]]) / 2 - 1L
  
  # we use data.table::fread() rather than readr::read_tsv() because of 
  # data.table.melt()
  datacols <- unlist(
    lapply(
      1L:n_topics,
      function(i) paste0(c("topic", "weight"), i))
  )
  doctopics <- data.table::fread(
    file = filename,
    skip = 1L,
    sep = "\t",
    col.names = c("doc_id", "doc_name", datacols, "dummy"),
    colClasses = c("integer", "character", rep(c("integer", "numeric"), times = n_topics), "integer")
  )
  doctopics[, "dummy" := NULL] # no idea where this dummy column comes from
  docnames <- doctopics[["doc_name"]] # keep for late use
  doctopics[, "doc_name" := NULL]
  
  retval <- melt(
    doctopics,
    measure.vars = list(
      paste0("topic", 1L:n_topics),
      paste0("weight", 1L:n_topics)
    )
  )
  retval[, "variable" := NULL]
  retval[, "value1" := retval$value1 + 1L]
  retval[, "doc_id" := retval$doc_id + 1L]
  setnames(retval, old = c("doc_id", "value1", "value2"), new = c("i", "j", "v"))
  setorderv(retval, cols = c("j", "v"), order = c(1L, -1L))
  setDF(retval)
  class(retval) <- "list"
  
  retval[["nrow"]] <- nrow(doctopics)
  retval[["ncol"]] <- max(retval[["j"]])
  retval[["dimnames"]] <- list(docnames, as.character(1L:retval[["ncol"]]))
  class(retval) <- "simple_triplet_matrix"
  retval
}
