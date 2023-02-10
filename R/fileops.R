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
#' # Restore model from binfile
#' data_dir <- system.file(package = "biglda", "extdata", "mallet")
#' binfile <- file.path(data_dir, "lda_mallet.bin")
#' model <- mallet_load_topicmodel(binfile)
#' 
#' # Restore model from instance- and statefile
#' model <- mallet_load_topicmodel(
#'   instancefile = file.path(data_dir, "instance_list.mallet"),
#'   statefile = file.path(data_dir, "lda_mallet.gz")
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
