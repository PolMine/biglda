// [[Rcpp::depends(RcppCWB)]]
#include <Rcpp.h>
#include <RcppCWB.h>

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <cstdlib>



//' Rcpp/RcppCWB implementation for writing token stream
//' 
//' @param corpus corpus.
//' @param p_attribute X
//' @param s_attribute X
//' @param registry X
//' @param filename X
//' @rdname rcppmetrics
//' @example
//' use("RcppCWB")
//' library(polmineR)
//' corpus("REUTERS")
//' fname <- tempfile(fileext = ".txt")
//' write_token_stream(
//'   corpus = "REUTERS",
//'   p_attribute = "word",
//'   strucs = 0:10,
//'   registry = ,
//'   filename = fname
//' )
// [[Rcpp::export]]
int write_token_stream(SEXP corpus, SEXP p_attribute, SEXP s_attribute, Rcpp::IntegerVector strucs, SEXP registry, Rcpp::StringVector filename) {
  
  int i, j, region_size;
  Rcpp::IntegerVector region(2);
  Rcpp::IntegerVector v(1);
  std::ofstream outdata;
  
  std::vector<int> strucs_int = Rcpp::as<std::vector<int> >(strucs);
  int strucs_length = strucs_int.size();
  Rcpp::StringVector s_attrs = RcppCWB::_cl_struc2str(
    corpus,
    s_attribute,
    strucs,
    registry)
  ;
  
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