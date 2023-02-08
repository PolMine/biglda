library(biglda)
if (isFALSE(mallet_is_installed())) mallet_install()
library(polmineR)

test_that(
  "FastCao2009",
  {
    fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
    lda_mallet <- mallet_load_topicmodel(fname)
    lda_topicmodels <- as_LDA(lda_mallet)
    me <- FastCao2009(lda_topicmodels)
    
    # This is the somewhat simplified code from ldatuning pkg (internal fn CaoJuan2009)
    # https://github.com/nikita-moor/ldatuning/blob/master/R/main.R
    m1 <- exp(lda_topicmodels@beta)
    pairs <- utils::combn(nrow(m1), 2)
    cos.dist <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      crossprod(x, y) / sqrt(crossprod(x) * crossprod(y))
    })
    ldatuning <- sum(cos.dist) / (lda_topicmodels@k*(lda_topicmodels@k-1)/2)
    
    expect_equal(me, ldatuning)
    
    cpp <- BigCao2009(lda_topicmodels@beta)
    expect_equal(cpp, me)
  }
)



test_that(
  "FastDeveaud2014",
  {
    fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
    lda_mallet <- mallet_load_topicmodel(fname)
    lda_topicmodels <- as_LDA(lda_mallet)
    me <- FastDeveaud2014(lda_topicmodels, parallel::detectCores() - 1L)
    
    # This is the somewhat simplified code from ldatuning pkg (internal fn CaoJuan2009)
    # https://github.com/nikita-moor/ldatuning/blob/master/R/main.R
    m1 <- exp(lda_topicmodels@beta)
    if (any(m1 == 0)) { m1 <- m1 + .Machine$double.xmin }
    pairs  <- utils::combn(nrow(m1), 2)
    jsd <- apply(pairs, 2, function(pair) {
      x <- m1[pair[1], ]
      y <- m1[pair[2], ]
      jsd <- 0.5 * sum(x*log(x/y)) + 0.5 * sum(y*log(y/x))
      return(jsd)
    })
    ldatuning_deveaud2014 <- sum(jsd) / (lda_topicmodels@k * (lda_topicmodels@k - 1))
    
    expect_identical(me, ldatuning_deveaud2014)
    
    expect_equal(
      FastDeveaud2014(lda_mallet, cl = parallel::detectCores() - 1L),
      FastDeveaud2014(lda_topicmodels, cl = parallel::detectCores() - 1L)
    )
    
    big <- BigDeveaud2014(lda_topicmodels@beta)
    expect_equal(me, big)
  }
)


test_that(
  "FastArun2010",
  {
    
    if (!mallet_is_installed()) mallet_install()
    library(polmineR)
    speeches <- polmineR::as.speeches(
      "GERMAPARLMINI",
      s_attribute_name = "speaker",
      s_attribute_date = "date",
      progress = FALSE
    )
    instance_list <- as.instance_list(speeches)
    lda <- BigTopicModel(n_topics = 25L, alpha_sum = 5.1, beta = 0.1)
    lda$addInstances(instance_list)
    lda$setNumThreads(1L)
    lda$setNumIterations(150L)
    lda$estimate()
    lda2 <- as_LDA(lda)
    myself <- FastArun2010(lda2, doclengths = summary(speeches)[["size"]])
    
    
    Arun2010 <- function(model, dtm) {
      # length of documents (count of words)
      len <- slam::row_sums(dtm)
      # evaluate metrics
      # matrix M1 topic-word
      m1 <- exp(model@beta) # rowSums(m1) == 1
      m1.svd <- svd(m1)
      cm1 <- as.matrix(m1.svd$d)
      # matrix M2 document-topic
      m2   <- model@gamma   # rowSums(m2) == 1
      cm2  <- len %*% m2    # crossprod(len, m2)
      norm <- norm(as.matrix(len), type="m")
      cm2  <- as.vector(cm2 / norm)
      # symmetric Kullback-Leibler divergence
      sum(cm1*log(cm1/cm2)) + sum(cm2*log(cm2/cm1))
    }
    
    reference <- Arun2010(model = lda2, as.DocumentTermMatrix(speeches, p_attribute = "word"))
    
    expect_identical(myself, reference)
    
    rcpp <- BigArun2010(
      beta = lda2@beta,
      gamma = lda2@gamma,
      doclengths = summary(speeches)[["size"]]
    )
    
    expect_equal(myself, rcpp)
  }
)


