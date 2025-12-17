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
//' @param attribute_type X
//' @param filename X
//' @rdname rcppmetrics
// [[Rcpp::export]]
int write_token_stream(SEXP corpus, SEXP p_attribute, SEXP s_attribute, SEXP registry, SEXP attribute_type, Rcpp::StringVector filename) {
  
  int i, n, region_size;
  Rcpp::IntegerVector region(2);
  std::ofstream outdata;

  n = RcppCWB::attribute_size(corpus, s_attribute, attribute_type, registry);

  outdata.open(filename[0]);
  if( !outdata ) {
    std::cerr << "Error: file could not be opened" << std::endl;
    exit(1);
  }
  
  for (i = 0; i < n; i++){
    region = RcppCWB::struc2cpos(corpus, s_attribute, registry, i);
    region_size = region[1] - region[0] + 1;

    Rcpp::IntegerVector cpos(region_size);
    cpos = Rcpp::seq(region[0], region[1]);

    Rcpp::StringVector values(region_size);
    values = RcppCWB::cpos2str(corpus, p_attribute, registry, cpos);
    
    int j;
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


//' Rcpp/RcppCWB implementation for writing token stream
//' 
//' @param corpus corpus.
//' @param p_attribute X
//' @param s_attribute X
//' @param registry X
//' @param attribute_type X
//' @param filename X
//' @rdname rcppmetrics
// [[Rcpp::export]]
int write_token_stream2(SEXP corpus, SEXP p_attribute, SEXP s_attribute, Rcpp::IntegerVector strucs, SEXP registry, SEXP attribute_type, Rcpp::StringVector filename) {
  
  int i, j, region_size;
  Rcpp::IntegerVector region(2);
  std::ofstream outdata;
  
  std::vector<int> strucs_int = Rcpp::as<std::vector<int> >(strucs);
  int strucs_length = strucs_int.size();
  
  outdata.open(filename[0]);
  if( !outdata ) {
    std::cerr << "Error: file could not be opened" << std::endl;
    exit(1);
  }
  
  for (i = 0; i < strucs_length; i++){
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