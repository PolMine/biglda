#' @exportMethod G
#' @docType methods
#' @rdname TopicModel-methods
setGeneric("G", function(x, ...) standardGeneric("G") )

#' @rdname TopicModel-methods
setMethod("G", "TopicModel", function(x) x@gamma) 

#' @exportMethod G<-
#' @rdname TopicModel-methods
setGeneric("G<-", function(x, value) standardGeneric("G<-"))


#' @exportMethod B<-
#' @rdname TopicModel-methods
setReplaceMethod("G", signature = "TopicModel", function(x, value) {
  stopifnot(
    is.matrix(value),
    ncol(value) == x@k
  )
  if (!is.null(x@documents)) stopifnot(length(x@documents) == nrow(value))
  if (!all(rownames(value) %in% x@documents))
    stop("all documents need to be present in value")
  
  x@gamma <- value[x@documents,]
  dimnames(x@gamma) <- NULL
  
  x
})
