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

//' Multiple testing following the graph
//'
//'
//' @param p_values  vector of p-values for the elementary hypothesis
//' @param log       TRUE: print log at each step; FALSE: silent
//'
//' @inheritParams c_mtp
//'
//' @return Hypothesis rejection status indicator vector
//'
//' @export
// [[Rcpp::export]]
IntegerVector c_mtp_single(NumericVector p_values, NumericVector alphas,
                           NumericMatrix mat_g, bool log = false) {

    NumericVector va = clone(alphas);
    NumericMatrix mg = clone(mat_g);

    int m = p_values.size();
    IntegerVector h_ind(m, 1);
    IntegerVector rst(m, 0);

    int n_h = m, fstop = 0;
    int i, j, l, k;
    double j_pal, pal;

    //testing algorithm
    while (n_h > 0 & 0 == fstop) {
        // print log
        if (log) {
            Rcout << "-------------------------- \n";
            Rcout << "----Rejection status: \n";
            Rf_PrintValue(rst);
            Rcout << "----Alphas:   \n";
            Rf_PrintValue(va);
            Rcout << "----p-values: \n";
            Rf_PrintValue(p_values);
            Rcout << "----G-Matrix: \n";
            Rf_PrintValue(mg);
        }

        // arg_min pval / alpha
        j      = 0;
        j_pal  = R_PosInf;
        for (i = 0; i < m; i ++) {
            if (0 == h_ind[i])
                continue;
            pal = p_values[i] / va[i];
            if (pal < j_pal) {
                j_pal = pal;
                j     = i;
            }
        }

        // test hypothesis j
        if (p_values[j] > va[j]) {
            fstop = 1;
            continue;
        }

        // update tests
        rst[j]   = 1;
        h_ind[j] = 0;
        n_h--;

        if (0 == n_h)
            continue;

        // update alpha
        for (i = 0; i < m; i ++) {
            if (0 == h_ind[i])
                continue;

            va[i] += va[j] * mg(j, i);
        }
        va[j] = 0;

        // update G
        for (l = 0; l < m; l++) {
            for (k = 0; k < m; k++) {
                if (0 == h_ind[l] |
                    0 == h_ind[k] |
                    l == k)
                    continue;

                mg(l, k) += mg(j, k) * mg(l, j);
                mg(l, k) /= 1 - mg(j, l) * mg(l, j);
            }
        }

        for (l = 0; l < m; l++) {
            for (k = 0; k < m; k++) {
                if (0 == h_ind[l] |
                    0 == h_ind[k] |
                    l == k)
                    mg(l, k) = 0.0;
            }
        }
    }

    // return
    return(rst);
}

//' Multiple testing following the graph
//'
//' @param alphas    vector of the original alphas
//' @param mat_g     transition matrix G
//'
//' @param p_values  matrix of p-values for the elementary hypothesis
//'
//' @return Hypothesis rejection status indicator vector
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix c_mtp(NumericMatrix p_values, NumericVector alphas,
                    NumericMatrix mat_g) {

    int m = p_values.nrow();
    int n = p_values.ncol();

    IntegerMatrix rst(m, n);
    std::fill(rst.begin(), rst.end(), 0);

    int i;
    for (i = 0; i < m; i++) {
        rst(i, _) = c_mtp_single(p_values(i, _), alphas, mat_g);
    }

    // return
    return(rst);
}


//' A single step in the multiple testing following the graph
//'
//'
//'
//' @return
//'
//' @export
// [[Rcpp::export]]
int c_mtp_step(NumericMatrix mat_g, NumericVector weights, IntegerVector h_ind,
               NumericVector p_values, double alpha) {

  int m = p_values.size();
  int rst;

  int i, j, l, k;
  double j_pal, pal;


  // arg_min pval / alpha
  j      = 0;
  j_pal  = R_PosInf;
  for (i = 0; i < m; i ++) {
    if (0 == h_ind[i])
      continue;
    pal = p_values[i] / (weights[i] * alpha);
    if (pal < j_pal) {
      j_pal = pal;
      j     = i;
    }
  }
  rst = j;

  // test hypothesis j
  if (p_values[j] > (weights[j] * alpha)) {
    return(-1);
  }

  // update tests
  h_ind[j] = 0;

  // update alpha
  for (i = 0; i < m; i ++) {
    if (0 == h_ind[i])
      continue;

    weights[i] += weights[j] * mat_g(j, i);
  }
  weights[j] = 0;

  // update G
  for (l = 0; l < m; l++) {
    for (k = 0; k < m; k++) {
      if (0 == h_ind[l] |
          0 == h_ind[k] |
          l == k)
        continue;

      mat_g(l, k) += mat_g(j, k) * mat_g(l, j);
      mat_g(l, k) /= 1 - mat_g(j, l) * mat_g(l, j);
    }
  }

  for (l = 0; l < m; l++) {
    for (k = 0; k < m; k++) {
      if (0 == h_ind[l] |
          0 == h_ind[k] |
          l == k)
        mat_g(l, k) = 0.0;
    }
  }

  // return
  return(rst);
}
