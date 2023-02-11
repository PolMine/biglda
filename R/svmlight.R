#' @param ... Additional arguments.
#' @rdname svmlight
setGeneric("svmlight", function(x, ...) standardGeneric("svmlight"))

#' Generate svmlight format
#' 
#' The svmlight data format is an input format used by Mallet.
#' 
#' Note that it is (currently) not possible to generate a (Mallet) `InstanceList`
#' from svmlight input that can be used for Mallet topic modelling: The Mallet
#' output from svmlight input is a `FeatureVector`, but Mallet topic modelling
#' requires a `FeatureSequence` as input.
#' 
#' @param x Input object, `DocumentTermMatrix` for the time being.
#' @param filename Path to a file.
#' @param verbose A `logical` value, whether to output progress messages.
#' @importFrom pbapply pblapply
#' @importFrom readr write_lines
#' @importFrom cli cli_progress_step
#' @rdname svmlight
#' @exportMethod svmlight
#' @examples
#' library(polmineR)
#' use("RcppCWB")
#' 
#' svmlight_file <- tempfile()
#' corpus("REUTERS") %>%
#'   as.DocumentTermMatrix(p_attribute = "word", s_attribute = "id") %>%
#'   svmlight(filename = svmlight_file)
setMethod("svmlight", "DocumentTermMatrix", function(x, filename, verbose = TRUE){
  
  if (verbose) cli_progress_step("split features by docs")
  features <- split(x = x$j, f = x$i)
  
  if (verbose) cli_progress_step("split values by docs")
  values <- split(x = x$v, f = x$i)
  
  if (verbose) cli_progress_step("make output vectors")
  lines <- pblapply(
    seq_along(features),
    function(i){
      index <- order(features[[i]])
      values <- sprintf("%s:%d", features[[i]][index], values[[i]][index])
      sprintf("%d %s", i, paste(values, collapse = " "))
    }
  )
  
  if (verbose) cli_progress_step("write to disk")
  write_lines(x = lines, file = filename, append = TRUE)
  
  return(invisible(TRUE))
})