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


//' @rdname rcppmetrics
// [[Rcpp::export]]
double BigDeveaud2014(const arma::mat & beta) {
  
  double aggr = 0;
  arma::mat B = arma::exp(beta).t();
  arma::mat MIN, MIN2, a, logged, d;
  arma::vec column;

  int j;
  for (j = 0; j < (B.n_cols - 1); j++){
    column = B.col(j);
    MIN = B.cols(j + 1, B.n_cols -1);

    MIN.for_each( [](arma::mat::elem_type& val) { val = 1 / val; } );
    MIN.each_col() %= column;
    a = column.t() * arma::log(MIN);
    aggr += arma::sum(arma::sum(a, 0)) * 0.5;

    MIN = B.cols(j + 1, B.n_cols -1);
    MIN2 = MIN;
    MIN2.each_col() /= column;
    logged = arma::log(MIN2);
    d = MIN % logged;
    aggr += arma::sum(arma::sum(d, 0)) * 0.5;
  }

  return aggr / (beta.n_rows * (beta.n_rows - 1));
}


// setMethod("FastDeveaud2014", "matrix", function(x, cl = parallel::detectCores() - 1L){
//   if (any(x == 0)) { x <- x + .Machine$double.xmin } # prevent NaN
//   x_t <- t(x)
//     
//     dist <- pblapply(
//         1L:(ncol(x_t) - 1L),
//         function(i){
//           m_min <- x_t[, -(1:i)]

//           a <- x_t[,i] * log(x_t[,i] / m_min)
//           b <- colSums(as.matrix(a))
//           c <- sum(b) * 0.5
//           
//           d <- m_min * log(m_min / x_t[,i])
//           e <- colSums(as.matrix(d))
//           f <- sum(e) * 0.5
//           
//           c + f
//         }
//     )
//     sum(unlist(dist)) / (nrow(x) * (nrow(x) - 1L))
// })
//   
//   
//   