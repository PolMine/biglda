library(polmineR)
library(biglda)
use("polmineR")

if (!mallet_is_installed()) mallet_install()

speeches <- polmineR::as.speeches(
  "GERMAPARLMINI",
  s_attribute_name = "speaker",
  s_attribute_date = "date"
)

instance_list <- as.instance_list(speeches)
instance_list_file <- path.expand("~/Lab/github/biglda/inst/extdata/mallet/instance_list.mallet")
instance_list$save(.jnew("java/io/file", instance_list_file))

lda <- BigTopicModel(n_topics = 25L, alpha_sum = 5.1, beta = 0.1)
lda$addInstances(instance_list)
lda$setNumThreads(1L)
lda$setTopicDisplay(50L, 10L)
lda$setNumIterations(150L)
lda$estimate()

statefile <- path.expand("~/Lab/github/biglda/inst/extdata/mallet/lda_mallet.gz")

lda$printState(rJava::.jnew("java/io/File", statefile))


lda2 <- BigTopicModel(n_topics = 25L, alpha_sum = 5.1, beta = 0.1)
lda2$initializeFromState(rJava::.jnew("java/io/File", statefile))
