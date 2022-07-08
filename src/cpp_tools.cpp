#include <Rcpp.h>
#include <Rmath.h>

using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

// [[Rcpp::init]]
void my_package_init(DllInfo *dll) {
  // initialization code here
  R_useDynamicSymbols(dll, TRUE);
}


// Test Rcpp function
//
//
// @param test test parameter
//
// @export
// [[Rcpp::export]]
double c_test(double test) {
  return pow(test,2);
}

// Get utility value by survival function
//
// @export
//
// [[Rcpp::export]]
NumericMatrix c_two_arm_diff(NumericMatrix dat) {

  int           i, nt = 0, nc = 0, n = dat.nrow();
  double        st = 0, sc = 0, my;
  NumericMatrix rst(n, 6);

  for (i = 0; i < n; i++) {
    if (1 == dat(i, 0)) {
      nt++;
      st += dat(i, 1);
      my = st / nt;
    } else {
      nc++;
      sc += dat(i, 1);
      my = sc / nc;
    }

    rst(i, 0) = i + 1;
    rst(i, 1) = nt;
    rst(i, 2) = st;
    rst(i, 3) = nc;
    rst(i, 4) = sc;
    rst(i, 5) = my;
  }


  // return
  return(rst);
}
