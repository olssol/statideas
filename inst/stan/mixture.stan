data {
  int<lower  = 0>  exist_y;
  int<lower  = 1>  exist_n;
  int<lower  = 0>  cur_y;
  int<lower  = 1>  cur_n;
  real<lower = 0, upper = 1>  weight;
}

parameters {
  real<lower = 0, upper = 1> theta;
}

model {
  target += log_mix(1 - weight,
                    uniform_lpdf(theta | 0, 1),
                    beta_lpdf(theta | exist_y + 1, exist_n - exist_y + 1));

  // likelihood
  cur_y ~ binomial(cur_n, theta);
}
