library(biglda)
if (!mallet_is_installed()) mallet_install()

library(polmineR)
speeches <- polmineR::as.speeches("GERMAPARLMINI", s_attribute_name = "speaker")

instance_list <- as.instance_list(speeches)
lda <- ParallelTopicModel(n_topics = 25L, alpha_sum = 5.1, beta = 0.1)
lda$addInstances(instance_list)
lda$setNumThreads(1L)
lda$setTopicDisplay(50L, 10L)
lda$setNumIterations(150L)
lda$estimate()
lda$write(
  rJava::.jnew("java/io/File", path.expand("~/Lab/github/biglda/inst/extdata/mallet/lda_mallet.bin"))
)
