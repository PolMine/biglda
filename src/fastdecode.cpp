// [[Rcpp::depends(RcppCWB)]]
#include <Rcpp.h>
#include <RcppCWB.h>

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <cstdlib>


//' Write input format for MALLET using RcppCWB 
//' 
//' MALLET and PartiallyCollapsedLDA use an input format with a line-wise corpus
//' format with one document per line, where each line contains a document ID,
//' a placeholder label, and the raw document text separated by tabs. This
//' function uses a fast Rcpp/RcppCWB implementation for decoding a CWB corpus
//' and writing it to the specified file.
//' 
//' @param corpus Length-one character vector with the ID of a CWB corpus.
//' @param p_attribute A p-attribute (positional attribute).
//' @param s_attribute The s-attribute delimiting documents.
//' @param registry Length-one character vector with the registry directory.
//' @param filename Name of output file (tilde expansion causes crash!).
//' @examples
//' library(biglda)
//' library(polmineR)
//' library(RcppCWB)
//' use("RcppCWB")
//' 
//'  fname <- tempfile(fileext = ".txt")
//'  size <- RcppCWB::attribute_size(
//'    corpus = "ALBB",
//'    attribute = "article_id",
//'    attribute_type = "s",
//'    registry = corpus_registry_dir("ALBB")
//' )
  
//' biglda::write_mallet_input(
//'   corpus = "ALBB",
//'   s_attribute = "article_id",
//'   p_attribute = "word",
//'   strucs = 0L:(size - 1L),
//'   registry = corpus_registry_dir("ALBB"),
//'   filename = fname
//' )
// [[Rcpp::export]]
int write_mallet_input(SEXP corpus, SEXP p_attribute, SEXP s_attribute, Rcpp::IntegerVector strucs, SEXP registry, Rcpp::StringVector filename) {
  
  int i, j, region_size;
  Rcpp::IntegerVector region(2);
  Rcpp::IntegerVector v(1);
  std::ofstream outdata;
  
  std::vector<int> strucs_int = Rcpp::as<std::vector<int> >(strucs);
  int strucs_length = strucs_int.size();
  
  SEXP att = RcppCWB::s_attr(corpus, s_attribute, registry);
  Rcpp::StringVector s_attrs = RcppCWB::struc_to_str(att, strucs);

  outdata.open(filename[0]);
  if( !outdata ) {
    std::cerr << "Error: file could not be opened" << std::endl;
    exit(1);
  }
  
  for (i = 0; i < strucs_length; i++){
    
    v[0] = i;
    outdata << s_attrs(i);
    outdata << "\tX\t";
    
    region = RcppCWB::struc2cpos(corpus, s_attribute, registry, strucs_int[i]);
    region_size = region[1] - region[0] + 1;
    
    Rcpp::IntegerVector cpos(region_size);
    cpos = Rcpp::seq(region[0], region[1]);
    
    Rcpp::StringVector values(region_size);
    values = RcppCWB::cpos2str(corpus, p_attribute, registry, cpos);
    
    for (j = 0; j < values.length(); j++){ 
      outdata << values(j);
      if (j < values.length() - 1){
        outdata << " ";
      }
    }
    outdata << std::endl;
  }
  outdata.close();
  
  return 0;
}