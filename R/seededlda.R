#' Write topic priors file
#' 
#' @params x A named list of character vectors
#' @examples
#' topic_priors <- list(
#'   "b" = c("oil", "barrel"),
#'   "2" = c("Bahrain")
#' )
#' fname <- tempfile(fileext = ".txt")
#' write_topic_priors(x = topic_priors, file = fname)
#' @export
write_topic_priors <- function(x, file){
  if (!is.list(x)) stop("input x required to be a list")
  if (is.null(names(x))) names(x) <- as.character(1:length(x))
  if (any(is.na(as.integer(names(x))))) stop("names required to be integer")
  
  vec <- unlist(lapply(Map(c, names(x), x), paste, collapse = ", "))
  writeLines(text = vec, con = file)
}


#' Read PCPLA topic means file
#' 
#' @importFrom stringi stri_sub stri_locate_all_regex
#' @examples
#' rundir <- "/Users/andreasblatte/Lab/tmp/seededlda/Runs/RunSuite2025-12-20--22_25_35/Run2025-12-20--22_25_35"
#' fname <- "doc_topic_means.csv"
#' m <- read_topic_means(file)
read_topic_means <- function(file){
  lines <- readLines(con = file)
  apply(
    stri_locate_all_regex(lines[[1]], "\\d,\\d{4}")[[1]],
    1L,
    function(x){
      values_char_raw <- stri_sub(lines, x[1], x[2])
      values_char <- chartr(",", ".", values_char_raw)
      as.numeric(values_char)
    })
}

#' @param fname File with top words, usually "topWords.txt".
#' @examples
#' rundir <- "/Users/andreasblatte/Lab/tmp/seededlda/Runs/RunSuite2025-12-20--22_25_35/Run2025-12-20--22_25_35/Spalias"
#' fname <- "TopWords.txt"
#' file <- file.path(rundir, fname)
#' read_top_words(file)
read_top_words <- function(file){
  lines <- readLines(con = file)
  df <- data.frame(strsplit(lines, split = ","))
  colnames(df) <- sprintf("topic_%d", 1L:ncol(df))
  df
}

#' @param fname File with top words, usually "topWords.txt".
#' @examples
#' rundir <- "/Users/andreasblatte/Lab/tmp/seededlda/Runs/RunSuite2025-12-20--22_25_35/Run2025-12-20--22_25_35/Spalias"
#' fname <- "RelevanceWords.txt"
#' file <- file.path(rundir, fname)
#' words <- read_relevance_words(file)
read_relevance_words <- function(file){
  lines <- readLines(con = file)
  df <- data.frame(strsplit(lines, split = ","))
  colnames(df) <- sprintf("topic_%d", 1L:ncol(df))
  df
}

#' @param fname File with term frequencies, usually "topWords.txt".
#' @examples
#' rundir <- "/Users/andreasblatte/Lab/tmp/seededlda/Runs/RunSuite2025-12-20--22_25_35/Run2025-12-20--22_25_35/Spalias"
#' fname <- "term_frequencies.txt"
#' file <- file.path(rundir, fname)
#' freq <- read_term_frequencies(file)
read_term_frequencies <- function(file){
  scan(file = file, what = integer())
}

#' @param fname File with term frequencies, usually "topWords.txt".
#' @examples
#' rundir <- "/Users/andreasblatte/Lab/tmp/seededlda/Runs/RunSuite2025-12-20--22_25_35/Run2025-12-20--22_25_35/Spalias"
#' fname <- "doc_lengths.txt"
#' file <- file.path(rundir, fname)
#' doc_lengths <- read_doc_lengths(file)
read_doc_lengths <- function(file){
  scan(file = file, what = integer())
}


#' Length of vectors corresponds to n words per document
#' 
#' @examples
#' fname <- "/Users/andreasblatte/Lab/tmp/seededlda/Runs/RunSuite2025-12-20--22_25_35/Run2025-12-20--22_25_35/Spalias/z_50.csv"
read_word_assignments <- function(fname){
  readLines(fname) %>% 
    strsplit(split = ",") %>% 
    lapply(as.integer)
}



