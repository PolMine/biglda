#' Fast Implementation of Cao et al. 2009
#' 
#' The function picks up the suggestion of Cao et al. 2009 for a density-based 
#' method for LDA model selection and offers an implementation that is 
#' substantially faster than ldatuning.
#' 
#' @param x A LDA model (S4 class from topicmodels package).
#' @export FastCao2009
#' @rdname FastCao2009
setGeneric("FastCao2009", function(x) standardGeneric("FastCao2009"))


#' @examples 
#' if (!mallet_is_installed()) mallet_install()
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet2.bin")
#' lda <- mallet_load_topicmodel(fname)
#' lda2 <- as_LDA(lda)
#' FastCao2009(lda2)
#' @rdname FastCao2009
setMethod("FastCao2009", "TopicModel", function(x){
  beta <- exp(x@beta)
  FastCao2009(beta)
})


#' @rdname FastCao2009
setMethod("FastCao2009", "matrix", function(x){
  cp <- crossprod(t(x))
  rtdg <- sqrt(diag(cp))
  almost <- tcrossprod(rtdg)
  yep <- cp / almost
  diag(yep) <- 0
  yep[lower.tri(yep)] <- 0
  sum(rowSums(yep)) / (nrow(x) * (nrow(x) - 1L) / 2)
})

#' @rdname FastCao2009
setMethod("FastCao2009", "jobjRef", function(x){
  beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
  FastCao2009(beta)
})


#' Fast Implementation of Deveaud 2014
#' 
#' @param x A topic model.
#' @param cl Number of cores to use
#' @importFrom parallel detectCores
#' @importFrom pbapply pblapply
#' @rdname FastDeveaud2014
#' @exportMethod FastDeveaud2014
setGeneric("FastDeveaud2014", function(x, cl) standardGeneric("FastDeveaud2014"))


#' @rdname FastDeveaud2014
#' @exportMethod FastDeveaud2014
setMethod("FastDeveaud2014", "matrix", function(x, cl = parallel::detectCores() - 1L){
  if (any(x == 0)) { x <- x + .Machine$double.xmin } # prevent NaN
  x_t <- t(x)
  dist <- pblapply(
    1L:(ncol(x_t) - 1L),
    function(i){
      m_min <- x_t[, -(1:i)]
      a <- x_t[,i] * log(x_t[,i] / m_min)
      b <- colSums(as.matrix(a))
      c <- sum(b) * 0.5
      
      d <- m_min * log(m_min / x_t[,i])
      e <- colSums(as.matrix(d))
      f <- sum(e) * 0.5
      
      c + f
    },
    cl = cl
  )
  sum(unlist(dist)) / (nrow(x) * (nrow(x) - 1L))
})



#' @examples
#' if (!mallet_is_installed()) mallet_install()
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet2.bin")
#' lda <- mallet_load_topicmodel(fname)
#' lda2 <- as_LDA(lda)
#' FastDeveaud2014(lda2, parallel::detectCores() - 1L)
#' @rdname FastDeveaud2014
#' @exportMethod FastDeveaud2014
setMethod("FastDeveaud2014", "TopicModel", function(x, cl = parallel::detectCores() - 1L){
  FastDeveaud2014(exp(x@beta), cl = cl)
})


#' @rdname FastDeveaud2014
#' @exportMethod FastDeveaud2014
setMethod("FastDeveaud2014", "jobjRef", function(x, cl = parallel::detectCores() - 1L){
  beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
  FastDeveaud2014(beta, cl = cl)
})


#' Fast Implementation of Arun 2014
#' 
#' @param x A topic model.
#' @param doclengths Integer vector.
#' @param gamma The gamma matrix.
#' @param ... Further arguments
#' @rdname FastArun2010
#' @exportMethod FastArun2010
setGeneric("FastArun2010", function(x, ...) standardGeneric("FastArun2010"))


#' @rdname FastArun2010
#' @exportMethod FastArun2010
#' @examples
#' \dontrun{
#' if (!mallet_is_installed()) mallet_install()
#' library(polmineR)
#' speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")
#' instance_list <- as.instance_list(speeches)
#' lda <- BigTopicModel(n_topics = 25L, alpha_sum = 5.1, beta = 0.1)
#' lda$addInstances(instance_list)
#' lda$setNumThreads(1L)
#' lda$setNumIterations(150L)
#' lda$estimate()
#' lda2 <- as_LDA(lda)
#' FastArun2010(lda2, doclengths = summary(speeches)[["size"]])
#' }
setMethod("FastArun2010", "TopicModel", function(x, doclengths){
  FastArun2010(x = exp(x@beta), gamma = x@gamma, doclengths = doclengths)
})


#' @rdname FastArun2010
#' @exportMethod FastArun2010
setMethod("FastArun2010", "matrix", function(x, gamma, doclengths){
  m1.svd <- svd(x)
  cm1 <- as.matrix(m1.svd$d)
  # matrix M2 document-topic
  cm2  <- doclengths %*% gamma    # crossprod(len, m2)
  norm <- norm(as.matrix(doclengths), type = "m")
  cm2  <- as.vector(cm2 / norm)
  # symmetric Kullback-Leibler divergence
  sum(cm1*log(cm1/cm2)) + sum(cm2*log(cm2/cm1))
})



#' @rdname FastArun2010
#' @exportMethod FastArun2010
#' @examples
#' if (!mallet_is_installed()) mallet_install()
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet2.bin")
#' lda <- mallet_load_topicmodel(fname)
#' FastArun2010(lda)
setMethod("FastArun2010", "jobjRef", function(x){
  beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
  gamma <- rJava::.jevalArray(x$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
  doclengths <- x$getDocLengthCounts()
  FastArun2010(x = beta, gamma = gamma, doclengths = doclengths)
})

