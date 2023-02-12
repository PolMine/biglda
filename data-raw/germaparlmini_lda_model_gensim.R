library(polmineR) # minimum v0.8.7.9012
library(slam)
library(reticulate)
gensim <- import("gensim")

use("polmineR")
coi <- "GERMAPARLMINI" # corpus of interest
language <- "german"

outdir <- "~/Lab/github/biglda/inst/extdata/gensim"
modelname <- "germaparlmini"

dtm <- corpus(coi) %>%
  as.speeches(s_attribute_name = "speaker", s_attribute_date = "date") %>%
  as.DocumentTermMatrix(p_attribute = "word")

# minimum document length 100 words
docs_to_drop_length <- which(slam::row_sums(dtm) < 100L) # less than 100
if (length(docs_to_drop_length) > 0L) dtm <- dtm[-docs_to_drop_length,]

# remove noisy words
noise_to_drop <- noise(colnames(dtm), specialChars = NULL, stopwordsLanguage = language)
noise_to_drop[["stopwords"]] <- c(
  noise_to_drop[["stopwords"]],
  capitalize(noise_to_drop[["stopwords"]])
)
dtm <- dtm[,-which(colnames(dtm) %in% unique(unlist(noise_to_drop)))]

# remove rare words
terms_to_drop_rare <- which(slam::col_sums(dtm) <= 10L)
if (length(terms_to_drop_rare) > 0L) dtm <- dtm[,-terms_to_drop_rare]

# remove documents that are empty now
empty_docs <- which(slam::row_sums(dtm) == 0L)
if (length(empty_docs) > 0L) dtm <- dtm[-empty_docs,]

saveRDS(object = dtm, file = path.expand(file.path(outdir, "germaparlmini_dtm.rds")))

gensim_corpus <- dtm_as_bow(dtm)

Encoding(dtm$dimnames$Terms) <- "UTF-8"
py$terms <- dtm$dimnames$Terms
py_run_string("token2id = dict(zip(terms, range(len(terms))))")
py$dictionary <- gensim$corpora$dictionary$Dictionary()
py_run_string("dictionary.__dict__['token2id'] = token2id")

lda_model <- gensim$models$ldamodel$LdaModel(
    corpus = gensim_corpus,
    id2word = py$dictionary,
    num_topics = 25L, 
    random_state = 100L,
    update_every = 1L,
    chunksize = 100L,
    passes = 10L,
    alpha = "auto",
    per_word_topics = TRUE
)

lda_model$save(path.expand(file.path(outdir, modelname)))
