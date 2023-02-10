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
  
  arma::mat X = arma::exp(beta);
  arma::vec s = arma::svd(X);
  arma::mat l = arma::conv_to<arma::mat>::from(doclengths).t();
  arma::mat M = l * gamma;
  M = M / l.max();
  arma::vec x2 = arma::conv_to<arma::vec>::from(M);
  
  double a = arma::sum(s % arma::conv_to<arma::vec>::from(arma::log(s / x2)));
  double b = arma::sum(x2 % arma::conv_to<arma::vec>::from(arma::log(x2 / s)));
  
  return a + b;
}


//' @rdname rcppmetrics
// [[Rcpp::export]]
double BigDeveaud2014(const arma::mat & beta) {
  
  double aggr = 0;
  arma::mat X = arma::exp(beta).t();
  arma::mat A, B, C, D, E, F;
  arma::vec column;
  
  A = X;
  A.for_each( [](arma::mat::elem_type& val) { val = 1 / val; } );
  
  int j;
  for (j = 0; j < (X.n_cols - 1); j++){
    column = X.col(j);
    C = A.cols(j + 1, A.n_cols - 1);
    C.each_col() %= column;
    E = column.t() * arma::log(C);
    aggr += arma::sum(arma::sum(E, 0)) * 0.5;

    C = X.cols(j + 1, X.n_cols - 1);
    D = C;
    D.each_col() /= column;
    F = C % arma::log(D);
    aggr += arma::sum(arma::sum(F, 0)) * 0.5;
  }

  return aggr / (beta.n_rows * (beta.n_rows - 1));
}

