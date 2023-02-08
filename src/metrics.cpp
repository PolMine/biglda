#include "RcppArmadillo.h"
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]

//' RcppArmadillo implementation of topic model metrics
//' 
//' @param X The beta matrix of an LDA topic model.
//' @rdname rcppmetrics
// [[Rcpp::export]]
double BigCao2009(const arma::mat & X) {
  arma::mat EXP = arma::exp(X);
  arma::mat CP = EXP * EXP.t();
  arma::vec d = arma::sqrt(CP.diag());
  arma::mat DIAG = d * d.t();
  arma::mat M = CP / DIAG;
  M.diag().zeros();
  arma::mat Y = arma::trimatu(M);
  return arma::accu(Y) / (Y.n_rows * (Y.n_rows - 1) / 2);
}


//' @param beta The beta matrix.
//' @param gamma The gamma matrix.
//' @param doclengths A `vector` with document lengths (number of tokens).
//' @rdname rcppmetrics
 // [[Rcpp::export]]
double BigArun2010(const arma::mat & beta, const arma::mat & gamma, arma::vec doclengths) {
  
  arma::mat B = arma::exp(beta);
  arma::vec d = arma::svd(B);
  
  arma::mat l = arma::conv_to<arma::mat>::from(doclengths).t();
  arma::mat M = l * gamma;
  M = M / l.max();
  arma::vec x2 = arma::conv_to<arma::vec>::from(M);
  
  double a = arma::sum(d % arma::conv_to<arma::vec>::from(arma::log(d / x2)));
  double b = arma::sum(x2 % arma::conv_to<arma::vec>::from(arma::log(x2 / d)));
  
  return a + b;
}

