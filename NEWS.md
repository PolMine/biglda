# biglda v0.0.2

* `mallet_instance_list_store()` renamed to `instance_list_save()` and
`mallet_instance_list_restore()` renamed to `instance_list_load()`.
* `as_LDA()` outputs information on number of docs / alphabet size.
* Method `svmlight()` moved here from package 'polmineR.misc'.
* Internal, unexported function `.mallet_cmd()` has been removed.
* `mallet_load_topicmodel()` will abort with an error, if no Mallet installation
is found.
* New functions `save_document_topics()` and `load_document_topics()` as more
memory-efficient way to get gamma matrix from Java to R.
* Function `load_word_weights()` now has argument `normalize` to reproduce 
Java/Mallet-side normalization when loading the matrix.
* `instance_list_load()` will check whether file exists.
* `svmlight()` reports document number at beginning of line.
* `gensim_ldamodel_as_LDA_Gibbs()` integrated as a `as_LDA()` method.
* Example Gensim topic model re-calculated with k = 25 (not 250).

# biglda v0.0.1

* `mallet_load_topicmodel()` includes some sanity checks to ensure that loading
topic model succeeds (report on JVM heap space, file size etc.).
* When loading the package, a message on the JVM heap space is shown.
* `as_LDA()` relies on 
* java class added to access model diagnostics for RTopicModel
* Added function `mallet_get_topic_model_diagnostics()` that will extract model diagnostics
  from RTopicModel java class object.
* Mapping the mallet-trained model on a class from the topicmodels class will now result in 
an object that can be loaded without requiring the presence of the biglda package (#2).
* Mapping the (mallet) model on the LDA_Gibbs model will be much faster because a method 
available for the RTopicModel class is used. This implies a general change from the 
ParallelTopicModel class to the RTopicModel class.
