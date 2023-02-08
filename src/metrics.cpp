#include "RcppArmadillo.h"
using namespace Rcpp;
// [[Rcpp::depends(RcppArmadillo)]]


// [[Rcpp::export]]
double BigCao2009(const arma::mat & X) {
  arma::mat EXP = arma::exp(X);
  arma::mat CP = EXP * EXP.t();
  arma::vec dsqrt = arma::sqrt(CP.diag());
  arma::mat almost = dsqrt * dsqrt.t();
  arma::mat M = CP / almost;
  M.diag().zeros();
  arma::mat Y = arma::trimatu(M);
  return arma::accu(Y) / (EXP.n_rows * (EXP.n_rows - 1) / 2);
}

