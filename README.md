Introducing the biglda package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Travis-CI Build
Status](https://api.travis-ci.org/PolMine/biglda.svg?branch=master)](https://travis-ci.org/PolMine/biglda)
[![codecov](https://codecov.io/gh/PolMine/biglda/branch/master/graph/badge.svg)](https://codecov.io/gh/PolMine/biglda/branch/master)

## Package for fast LDA topic modelling for big corpora.

The *ParallelTopicModel* class from the Machine Learning for Language
Toolkit ‘mallet’ offers a fast implementation of an LDA topic model that
yields good results. The purpose of the ‘biglda’ package is to offer a
seamless and fast interface to the Java classes of ‘mallet’ so that the
multicore implementation of the LDA algorithm can be used. The
`as_LDA()` function can be used to map the mallet model on the
`LDA_Gibbs` class from the widely used `topicanalysis` package.

The mallet Java package is not shipped with the “biglda” package. It
needs to be installed explicitly using the `mallet_install()` function.
If mallet is not yet available, a warning will be issued upon loading
biglda to install mallet. The R package “mallet” that has been around
for a while it the traditional interface to the “mallet” Java package
for R users. However, the a [RTopicModel
class](https://github.com/mimno/Mallet/blob/af1fcb1f3e6561afac28f4331e4e0d735b3d11b4/src/cc/mallet/topics/RTopicModel.java)
is used as an interface to the `ParallelTopicModel` class, hiding the
method to define the number of cores to be used from the R used, thus
limiting the potential to speed up the computation of a topic model
using multiple threads. What is more, functionality for full access to
the computed model is hidden, inhibiting the extraction of the
information the mallet LDA topic model that is required to map the topic
model on the LDA_Gibbs topic model. Note that it is not possible to use
the R packages “biglda” and “mallet” in parallel. If “mallet” is loaded,
it will put its Java Archive on the classpath of the Java Virtual
Machine (JVM), making the latest version of the `ParallelTopicModel`
class inaccessible and causing errors that may be difficult to
understand. Therefore, a warning will be issued when “biglda” may detect
that the JAR included in the “mallet” package is in the classpath.

## Useful sources

David Minmo, “The Details: Training and Validating Big Models on Big
Data”:
<http://journalofdigitalhumanities.org/2-1/the-details-by-david-mimno/>
