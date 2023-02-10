library(biglda)
if (isFALSE(mallet_is_installed())) mallet_install()
library(polmineR)
library(rJava)

test_that(
  "",
  {
    speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker", s_attribute_date = "date")
    
    
    instance_list1 <- as.instance_list(speeches, p_attribute = "word")

    id_list <- get_token_stream(speeches, p_attribute = "word", decode = FALSE)
    instance_list2 <- as.instance_list(id_list, corpus = "GERMAPARLMINI")

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
    bin <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin")
    model <- mallet_load_topicmodel(bin)

    # This is an approach that uses a (temporary) file written
    # to disk. The advantage is that it is a sparse matrix that is
    # passed
    fname <- save_word_weights(model)
    beta_1 <- load_word_weights(normalize = FALSE, fname)

    # This is the call used internally by 'as_LDA()'. The difference
    # is that the arguments of the $getTopicWords()-method are FALSE
    # (argument 'normalized') and TRUE (argument 'smoothed')
    beta_2 <- rJava::.jevalArray(model$getTopicWords(FALSE, TRUE), simplify = TRUE)
    alphabet <- strsplit(model$getAlphabet()$toString(), "\n")[[1]]
    colnames(beta_2) <- alphabet
    rownames(beta_2) <- as.character(1:nrow(beta_2))
    
    # Demonstrate the equivalence of the two approaches
    identical(rownames(beta_1), rownames(beta_2))
    identical(colnames(beta_1), colnames(beta_2))
    identical(apply(beta_1, 1, order), apply(beta_2, 1, order))
    testthat::expect_equal(beta_1, beta_2)
  }
)

test_that(
  "save and load document topics",
  {
    model <- system.file(package = "biglda", "extdata", "mallet", "lda_mallet.bin") |>
      mallet_load_topicmodel()
    
    y <- save_document_topics(model) |>
      load_document_topics(verbose = FALSE)
    
    y2 <- .jevalArray(model$getDocumentTopics(TRUE, TRUE), simplify = TRUE)
    dimnames(y2) <- list(model$getDocumentNames(), as.character(1L:ncol(y2)))
    
    expect_equal(y, y2)
    
    # -------------------------
    
    beta1 <- save_word_weights(model, verbose = FALSE) |>
      load_word_weights(normalize = FALSE, verbose = FALSE)

    beta2 <- .jevalArray(model$getTopicWords(FALSE, TRUE), simplify = TRUE)
    rownames(beta2) <- as.character(1L:nrow(beta2))
    colnames(beta2) <- strsplit(model$getAlphabet()$toString(), split = "\n")[[1]]

    expect_equal(beta1, beta2)
    
    # check that R-side normalization yields same result as Mallet/Java --------
    
    beta1 <- save_word_weights(model, verbose = FALSE) |>
      load_word_weights(normalize = TRUE, verbose = FALSE)

    beta2 <- .jevalArray(model$getTopicWords(TRUE, TRUE), simplify = TRUE)
    dimnames(beta2) <- list(
      as.character(1L:nrow(beta2)),
      strsplit(model$getAlphabet()$toString(), split = "\n")[[1]]

    )

    expect_equal(beta1, beta2)
  }
)

