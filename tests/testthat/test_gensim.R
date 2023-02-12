library(biglda)
if (isFALSE(mallet_is_installed())) mallet_install()
library(polmineR)
library(rJava)

test_that(
  "",
  {
    skip_if_not_installed(pkg = "reticulate")
    skip_if_not(reticulate::py_module_available("gensim"))

    gensim <- reticulate::import("gensim")

    dir <- system.file(package = "biglda", "extdata", "gensim")
    dtmfile <- file.path(dir, "germaparlmini_dtm.rds")

    gensimmodel <- gensim_ldamodel_load(
      modeldir = dir,
      modelname = "germaparlmini"
    )
    rmodel1 <- as_LDA(gensimmodel, dtm = readRDS(dtmfile))
    
    topics_terms <- topicmodels::get_terms(rmodel1, 10)
    docs_topics <- topicmodels::get_topics(rmodel1, 5)
    
    if (requireNamespace("RcppCNPy", quietly = TRUE)){
      beta <- RcppCNPy::npyLoad(file.path(dir, "germaparlmini.expElogbeta.npy"))
      expect_identical(dim(rmodel1@beta), dim(beta))
      # expect_equal(exp(rmodel1@beta) - 1, beta)
    } 

  }
)

