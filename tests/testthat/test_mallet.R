library(biglda)
if (isFALSE(mallet_is_installed())) mallet_install()
library(polmineR)
library(rJava)

test_that(
  "",
  {
    use("polmineR", corpus = "GERMAPARLMINI")
    speeches <- corpus("GERMAPARLMINI") %>%
      as.speeches(s_attribute_name = "speaker", s_attribute_date = "date")
    
    instance_list1 <- as.instance_list(speeches, p_attribute = "word", verbose = FALSE)

    id_list <- get_token_stream(speeches, p_attribute = "word", decode = FALSE)
    vocab <- p_attributes("GERMAPARLMINI", p_attribute = "word")
    instance_list2 <- as.instance_list(
      id_list,
      vocabulary = vocab,
      docnames = names(id_list)
    )

    expect_identical(instance_list1$size(), instance_list2$size())
    
    alphabet1 <- strsplit(instance_list1$getDataAlphabet()$toString(), "\n")[[1]] 
    alphabet2 <- strsplit(instance_list2$getDataAlphabet()$toString(), "\n")[[1]] 
    expect_identical(alphabet1, alphabet2)
    
    n_token1 <- sapply(0L:(instance_list1$size() - 1L), function(i) length(instance_list1$get(i)$getData()$getFeatures()))
    expect_identical(unname(sapply(speeches@objects, size)), n_token1)
    
    n_token2 <- sapply(0L:(instance_list2$size() - 1L), function(i) length(instance_list2$get(i)$getData()$getFeatures()))
    expect_identical(unname(sapply(id_list, length)), n_token2)
    
    expect_identical(n_token1, n_token2)
    
    FeatureSequence1 <- instance_list1$get(0L)$getData()$getFeatures()
    FeatureSequence2 <- instance_list2$get(0L)$getData()$getFeatures()
    
    expect_identical(FeatureSequence1, FeatureSequence2)
  }
)

test_that(
  "check getDocLengthCounts()-method of BigTopicModel",
  {
    speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker", s_attribute_date = "date")
    instance_list <- as.instance_list(speeches)
    lda <- BigTopicModel(n_topics = 25L, alpha_sum = 5.1, beta = 0.1)
    lda$addInstances(instance_list)
    expect_identical(lda$getDocLengthCounts(), summary(speeches)[["size"]])
    
    fname <- tempfile()
    lda$write(rJava::.jnew("java/io/File", fname))
    model_reloaded <- mallet_load_topicmodel(fname)
    model_reloaded$getDocLengthCounts()
    expect_identical(lda$getDocLengthCounts(), model_reloaded$getDocLengthCounts())
  }
)


test_that(
  "result of save_document_topics()/load_document_topics() equal to $getDocumentTopics()",
  {
    binfile <- system.file(
      package = "biglda",
      "extdata", "mallet", "lda_mallet.bin"
    )
    
    model <- mallet_load_topicmodel(binfile)
    tmpfile <- save_document_topics(model)
    doctopics1 <- load_document_topics(tmpfile)

    doctopics2 <- rJava::.jevalArray(
      model$getDocumentTopics(TRUE, TRUE),
      simplify = TRUE
    )
    dimnames(doctopics2) <- list(
      model$getDocumentNames(),
      as.character(1L:ncol(doctopics2))
    )

    testthat::expect_identical(dim(doctopics1), dim(doctopics2))
    testthat::expect_equal(doctopics1, doctopics2)
  }
)

test_that(
  "result of save_document_topics()/load_document_topics() equal to $getDocumentTopics()",
  {
    model <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin") |>
      mallet_load_topicmodel()
    
    y <- save_document_topics(model) |>
      load_document_topics(verbose = FALSE)
    
    y2 <- .jevalArray(model$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
    dimnames(y2) <- list(model$getDocumentNames(), as.character(1L:ncol(y2)))
    
    expect_equal(y, y2)
  }
)

test_that(
  "save and load document topics",
  {
    model <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin") |>
      mallet_load_topicmodel()
    
    # -------------------------
    
    beta_xl <- save_word_weights(model, minimized = FALSE, verbose = FALSE) |>
      load_word_weights(minimized = FALSE, normalize = FALSE, verbose = FALSE)
    
    beta_min <- save_word_weights(model, minimized = TRUE, verbose = FALSE) |>
      load_word_weights(minimized = TRUE, normalize = FALSE, beta_coeff = 0.1, verbose = TRUE)
    
    beta_min <- beta_min[, colnames(beta_xl)]
    
    expect_equal(beta_xl, beta_min)
    
    # -------------------------

    beta1 <- save_word_weights(model, verbose = FALSE) |>
      load_word_weights(normalize = FALSE, verbose = FALSE)

    beta2 <- .jevalArray(model$getTopicWords(FALSE, TRUE), simplify = TRUE)
    rownames(beta2) <- as.character(1L:nrow(beta2))
    colnames(beta2) <- strsplit(model$getAlphabet()$toString(), split = "\n")[[1]]
    
    beta1 <- beta1[, colnames(beta2)]

    expect_equal(beta1, beta2)
    
    # check that R-side normalization yields same result as Mallet/Java --------
    
    beta1 <- save_word_weights(model, verbose = FALSE) |>
      load_word_weights(normalize = TRUE, verbose = FALSE)

    beta2 <- .jevalArray(model$getTopicWords(TRUE, TRUE), simplify = TRUE)
    dimnames(beta2) <- list(
      as.character(1L:nrow(beta2)),
      strsplit(model$getAlphabet()$toString(), split = "\n")[[1]]

    )
    
    beta1 <- beta1[, colnames(beta2)]
    expect_equal(beta1, beta2)
  }
)

test_that(
  "equivalence of different ways to as_LDA()",
  {
    data_dir <- system.file(package = "biglda", "extdata", "mallet")
    
    BTM <- mallet_load_topicmodel(
      instancefile = file.path(data_dir, "instance_list.mallet"),
      statefile = file.path(data_dir, "lda_mallet.gz")
    )

    LDA <- as_LDA(BTM, verbose = FALSE)

    LDA2 <- as_LDA(BTM, beta = matrix(), gamma = matrix(), verbose = FALSE)
    
    B(LDA2) <- save_word_weights(BTM, minimized = TRUE) |>
      load_word_weights(minimized = TRUE) |>
      log()
    
    G(LDA2) <- save_document_topics(BTM) |>
      load_document_topics()
    
    LDA@control@prefix <- character()
    LDA2@control@prefix <- character()
    expect_equal(LDA, LDA2)

  }
)
