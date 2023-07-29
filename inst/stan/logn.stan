data {
  int<lower=0>  N;
  vector[N]     y;
  real<lower=0> L_m;
  real<lower=0> U_m;
  real<lower=0> L_cv;
  real<lower=0> U_cv;
}

parameters {
  real<lower=0> m;
  real<lower=0> cv;
}

transformed parameters {
  real          mu;
  real<lower=0> sigma;

  mu    = log(m / sqrt(1 + cv^2));
  sigma = sqrt(log(1 + cv^2));
}

model {
  m      ~ uniform(L_m,  U_m);
  cv     ~ uniform(L_cv, U_cv);
  log(y) ~ normal(mu,    sigma);
}

generated quantities {
  real          x_tilde;
  real<lower=0> y_tilde;

  x_tilde = normal_rng(mu, sigma);
  y_tilde = exp(x_tilde);
}
