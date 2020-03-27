library(biglda)
if (isFALSE(mallet_is_installed())) mallet_install()
library(polmineR)

test_that(
  "FastCao2009",
  {
    fname <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet2.bin")
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
    
    expect_identical(me, ldatuning)
  }
)

