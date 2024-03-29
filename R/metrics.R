#' Evaluate topic model.
#' @param x Input object of class inheriting from `TopicModel`.
#' @param verbose A `logical` value, whether to output progress messages.
#' @param ... Further arguments (unused).
#' @exportMethod metrics
#' @docType methods
#' @rdname metrics
setGeneric("metrics", function(x, ...) standardGeneric("metrics") )

#' @examples 
#' lda <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin") |>
#'   mallet_load_topicmodel() |>
#'   as_LDA()
#' m <- metrics(lda)
#' if (interactive()) plot(m)
#' @rdname metrics
setMethod("metrics", signature = "TopicModel", function(x, verbose = TRUE){
  
  stopifnot(
    length(verbose) == 1L, is.logical(verbose)
  )
  
  if (verbose) cli_progress_step("calculating cao2009")
  cao <- BigCao2009(X = B(x))
  
  if (sum(dim(G(x))) > 0L && length(x@doclengths) > 0L){
    if (verbose) cli_progress_step("calculating arun2010")
    arun <- BigArun2010(beta = B(x), gamma = G(x), doclengths = x@doclengths)
  } else {
    if (verbose) cli_alert_info("gamma matrix and/or doclengths not present - skipping Arun2010")
    arun <- NA
  }
  
  if (verbose) cli_progress_step("calculating deveaud2014")
  deveaud <- BigDeveaud2014(beta = B(x))
  
  retval <- data.frame(
    k = x@k,
    cao2009 = cao,
    arun2010 = arun,
    deveaud2014 = deveaud
  )
  
  class(retval) <- c("metrics", class(retval))
  retval
})



#' Plot metrics.
#' 
#' Basic plot to visualize metrics. For a nicer, more elaborate plot, see
#' `ldatuning::FindTopicsNumber_plot()`
#' @importFrom graphics legend lines par points
#' @rdname metrics
#' @exportS3Method 
plot.metrics <- function(x, ...){
  
  metrics <- c("cao2009", "arun2010", "deveaud2014")
  if (!all(metrics %in% colnames(x))) stop("all metrics need to be present")
  
  for (m in metrics){
    x[[m]] <- (x[[m]] - min(x[[m]])) / max(x[[m]] - min(x[[m]]))
  }
  
  mfrow_old <- par("mfrow")
  par(mfrow = c(1,2))
  plot(
    x = x$k, y = x$cao2009,
    ylab = "normalized metric", xlab = "k (number of topics)",
    main = "Metrics to minimize",
    type = "n"
  )
  lines(x = x$k, y = x$arun2010, col = "steelblue", lwd = 1)
  points(x = x$k, y = x$arun2010, col = "steelblue", pch = 18)
  lines(x = x$k, y = x$cao2009, col = "chartreuse3", lwd = 1)
  points(x = x$k, y = x$cao2009, col = "chartreuse3", pch = 17)
  legend(
    x = "topleft",
    legend = c("arun2010", "cao2009"),
    fill = c("steelblue", "chartreuse3"),
    cex = 0.5
  )
  
  plot(
    x = x$k, y = x$cao2009,
    ylab = "normalized metric", xlab = "k (number of topics)", 
    main = "Metrics to maximize",
    type = "n"
  )
  lines(x = x$k, y = x$deveaud2014, col = "coral2", lwd = 1)
  points(x = x$k, y = x$deveaud2014, col = "coral2", pch = 19)
  legend(x = "topright", legend = "deveaud2014", fill = "coral2", cex = 0.5)
  
  par(mfrow = mfrow_old)
}

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
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
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
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
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
#' use("polmineR")
#' speeches <- corpus("GERMAPARLMINI") %>%
#'   as.speeches(s_attribute_name = "speaker", s_attribute_date = "date")
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
#' fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
#' lda <- mallet_load_topicmodel(fname)
#' FastArun2010(lda)
setMethod("FastArun2010", "jobjRef", function(x){
  beta <- rJava::.jevalArray(x$getTopicWords(TRUE, TRUE), simplify = TRUE) 
  gamma <- rJava::.jevalArray(x$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
  doclengths <- x$getDocLengthCounts()
  FastArun2010(x = beta, gamma = gamma, doclengths = doclengths)
})


#' Get topic model diagnostics.
#' 
#' The MALLET topic model toolkit includes a class `TopicModelDiagnostics` able
#' to prepare a set of metrics on the topics of a topic model. The function
#' `get_diagnostics()` will return a `data.table` with these
#' diagnostics. See the [Mallet
#' documentation](http://mallet.cs.umass.edu/diagnostics.php) for an explanation
#' of the metrics.
#' @param x An instance of the RTopicModel class (java object).
#' @param n An `integer` value, the number of the top words that will be evaluated.
#' @param verbose A `logical` value, whether to show progress messages.
#' @return A `data.table`.
#' @examples 
#' f <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
#' model <- mallet_load_topicmodel(f)
#' diagnostics <- get_diagnostics(model)
#' head(diagnostics)
#' @importFrom data.table := rbindlist
#' @importFrom xml2 read_xml xml_find_all xml_attrs
get_diagnostics <- function(x, n = 50L, verbose = TRUE){
  stopifnot(
    is.numeric(n),
    is.logical(verbose)
  )
  if (x$getClass()$toString() != "class BigTopicModel"){
    warning("Input expected to be a 'BigTopicModel' Java class object.")
  }
  
  if (verbose) cli_progress_step("instantiate diagnostics class")
  model_diagnostics <- rJava::.jnew("BigTopicModelDiagnostics", x, as.integer(n))
  
  if (verbose) cli_progress_step("retrieve and parse diagnostics")
  stri <- model_diagnostics$toXML()
  xml <- xml2::read_xml(stri)
  nodes <- xml2::xml_find_all(xml, xpath = "/model/topic")
  dt <- rbindlist(
    lapply(
      xml_attrs(nodes),
      function(x) as.data.table(as.list(setNames(as.numeric(x), names(x))))
    )
  )
  dt[, "id" := as.integer(dt[["id"]])]
  dt
}
