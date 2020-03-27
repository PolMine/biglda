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
#' @param model A LDA model.
#' @param cl Number of cores to use
#' @importFrom parallel detectCores
#' @importFrom pbapply pblapply
FastDeveaud2014 <- function(model, cl = parallel::detectCores() - 1L){
  m1 <- exp(model@beta)
  if (any(m1 == 0)) { m1 <- m1 + .Machine$double.xmin } # prevent NaN
  m2 <- t(m1)
  dist <- pblapply(
    1L:(ncol(m2) - 1L),
    function(i){
      print(i)
      m_min <- m2[, -(1:i)]
      a <- m2[,i] * log(m2[,i] / m_min)
      b <- colSums(as.matrix(a))
      c <- sum(b) * 0.5
      
      d <- m_min * log(m_min / m2[,i])
      e <- colSums(as.matrix(d))
      f <- sum(e) * 0.5
      
      c + f
    },
    cl = cl
  )
  retval <- sum(unlist(dist)) / (model@k * (model@k - 1L))
}
