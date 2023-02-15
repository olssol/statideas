data {
  int<lower  = 1>  ns;
  int<lower  = 0>  y[ns];
  int<lower  = 0>  n[ns];
  real<lower = 0>  pri_sig;
}

parameters {
  real            beta[ns];
  real            mu_beta;
  real<lower = 0> sigma;
}

transformed parameters {
  real<lower = 0, upper = 1> theta[ns];
  for (i in 1:ns) {
    theta[i] = exp(beta[i]) / (1 + exp(beta[i]));
  }
}

model {
  beta    ~ normal(mu_beta, sigma);
  mu_beta ~ normal(0, 100);
  sigma   ~ normal(0, pri_sig);

  // likelihood
  y ~ binomial(n, theta);
}
