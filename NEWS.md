biglda 0.0.0.9003
=================
* java class added to access model diagnostics for RTopicModel

biglda 0.0.0.9002
=================

* Mapping the mallet-trained model on a class from the topicmodels class will now result in 
an object that can be loaded without requiring the presence of the biglda package (#2).
* Mapping the (mallet) model on the LDA_Gibbs model will be much faster because a method 
available for the RTopicModel class is used. This implies a general change from the 
ParallelTopicModel class to the RTopicModel class.
