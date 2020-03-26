#' Fast Implementation of Cao et al. 2009
#' 
#' The function picks up the suggestion of Cao et al. 2009 for a density-based 
#' method for LDA model selection and offers an implementation that is 
#' substantially faster than ldatuning.
#' 
#' @param model A LDA model (S4 class from topicmodels package).
#' @export density
#' @rdname density
density <- function(model){
  beta_exp <- exp(model@beta)
  cp <- crossprod(t(beta_exp))
  rtdg <- sqrt(diag(cp))
  almost <- tcrossprod(rtdg)
  yep <- cp / almost
  diag(yep) <- 0
  yep[lower.tri(yep)] <- 0
  metric <- sum(rowSums(yep)) / (model@k*(model@k-1)/2)
  metric
}

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
