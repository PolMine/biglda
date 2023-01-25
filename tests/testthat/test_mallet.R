library(biglda)
if (isFALSE(mallet_is_installed())) mallet_install()
library(polmineR)

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


