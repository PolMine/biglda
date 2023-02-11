Introducing the biglda package
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R build
status](https://github.com/PolMine/biglda/workflows/R-CMD-check/badge.svg)](https://github.com/PolMine/biglda/actions)
[![codecov](https://codecov.io/gh/PolMine/biglda/branch/master/graph/badge.svg)](https://codecov.io/gh/PolMine/biglda/branch/master)

## Package for fast LDA topic modelling for big corpora.

Topic modelling is an established and useful unsupervised learning
technique for detecting topics in large collections of text. Among the
many variants of topic modelling and innovations that are been tried,
the classic ‘Latent Dirichlet Allocation’ (LDA) algorithm remains to be
a powerful tool that yields good results. Yet computing LDA topic models
requires significant system resources, particularly when corpora grow
large. The biglda package addresses three specific issues that may be
outright obstacles to using LDA topic models productively and in a
state-of-the-art fashion.

The package addresses issues of computing time and RAM limitations at
the different relevant stages when working with topic models:

- *Data preparation and import*: Issues of performance and memory
  efficiency start to arise when preparing input data for a training
  algorithm.

- *Fitting topic models*: Computing time for a single topic model may be
  extensive. It is good practice to evaluate a set of topic models for
  hyperparameter optimization. So you need a fast implementation of LDA
  topic modelling to be able to fit a set of topic models within
  reasonable time.

- *Evaluating topic models*: Issues performance and memory efficiency
  are relevant when computing indicators to assess topic models may be
  considerable. Computations require mathematical operations with really
  large matrices. Memory may be exhausted easily.

More plastically spoken: As you work with increasingly large data,
fitting and evaluating topic models can bring hours and days you wait
for a result to emerge, just to see that the process crashes because
memory has been exhausted.

The biglda package addresses performance and memory efficiency issues
for R users at all stages. The *ParallelTopicModel* class from the
Machine Learning for Language Toolkit ‘mallet’ offers a fast
implementation of an LDA topic model that yields good results. The
purpose of the ‘biglda’ package is to offer a seamless and fast
interface to the Java classes of ‘mallet’ so that the multicore
implementation of the LDA algorithm can be used. The `as_LDA()` function
can be used to map the mallet model on the `LDA_Gibbs` class from the
widely used `topicanalysis` package.

## Installation

### ‘biglda’ package

The biglda package is a GitHub-only package at this time. Install the
stable version as follows.

``` r
remotes::install_github("PolMine/biglda")
```

Install the development version of the package as follows.

``` r
remotes::install_github("PolMine/biglda", ref = "dev")
```

### Installing Mallet

The focus of the biglda package is to offer a seamless interface to
Mallet. The `biglda::mallet_install()` function installs Mallet
(v202108) in a directory within the package. The big disadvantage of
this installation mechanism is that the Mallet installation is
overwritten whenever you install a new version of biglda, and Mallet
needs to be installed anew. This is why installing Mallet in the storage
location used by your system for add-on software (such as “/opt” on
Linux/macOS) is recommended.

We strongly recommend to install the latest version of Mallet (v202108
or higher). Among others, it includes a new
`$printDenseDocumentTopics()` method of the `ParallelTopicModel` used by
`biglda::save_document_topics()`, improving the efficiency of moving
data from Java/Mallet to R significantly.

Note that v202108 is a “serialization-breaking release”. Instance files
and binary models prepared using previous versions cannot be processed
with this version and may have to be rebuilt.

On Linux and macOS machines, you may use the following lines of code for
installing Mallet. Note that admin privileges (“sudo”) may be required.

``` sh
mkdir /opt/mallet
cd /opt/mallet
wget https://github.com/mimno/Mallet/releases/download/v202108/Mallet-202108-bin.tar.gz
tar xzfv Mallet-202108-bin.tar.gz
rm Mallet-202108-bin.tar.gz
```

### Installing rJava

``` r
R CMD javareconf
```

## Using biglda

### Loading biglda

Note that it is not possible to use the R packages “biglda” and “mallet”
in parallel. If “mallet” is loaded, it will put its Java Archive on the
classpath of the Java Virtual Machine (JVM), making the latest version
of the `ParallelTopicModel` class inaccessible and causing errors that
may be difficult to understand. Therefore, a warning will be issued when
“biglda” may detect that the JAR included in the “mallet” package is in
the classpath.

``` r
options(java.parameters = "-Xmx8g")
Sys.setenv(MALLET_DIR = "/opt/mallet/Mallet-202108")
library(biglda)
```

    ## Mallet version: v202108

    ## JVM memory allocated: 7.1 Gb

### A sample workflow

## Related work

The R package “mallet” has been around for a while and is the
traditional interface to the “mallet” Java package for R users. However,
the a [RTopicModel
class](https://github.com/mimno/Mallet/blob/af1fcb1f3e6561afac28f4331e4e0d735b3d11b4/src/cc/mallet/topics/RTopicModel.java)
is used as an interface to the `ParallelTopicModel` class, hiding the
method to define the number of cores to be used from the R used, thus
limiting the potential to speed up the computation of a topic model
using multiple threads. What is more, functionality for full access to
the computed model is hidden, inhibiting the extraction of the
information the mallet LDA topic model that is required to map the topic
model on the LDA_Gibbs topic model.

- [mallet](https://CRAN.R-project.org/package=mallet) R package
- [dfrtopics](https://github.com/agoldst/dfrtopics)

## Discussion

The biglda package is designed for heavy-duty work with large data sets.
If you work with smaller data, other approaches and implementations that
are established and that may be easier to use and to install (the rJava
Blues!)  
do a very good job indeed. But we are not just happy to also have
biglda. Jobs with big data may just take too long or simply fail because
memory is exhausted. We think that biglda pushes the frontier of being
able of productively using topic modelling in the R realm.

(Alternative: Command line use of Mallet. Quarto.)

## Useful sources

David Minmo, “The Details: Training and Validating Big Models on Big
Data”:
<http://journalofdigitalhumanities.org/2-1/the-details-by-david-mimno/>
