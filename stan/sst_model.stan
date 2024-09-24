data {
  int<lower=1> N;  // number of observations
  int<lower=1> P;  // number of participants
  array[N] int<lower=1, upper=P> participant;  // participant id for each trial
  vector[N] rt;  // standardized reaction time
  vector[N] ssd;  // standardized stop-signal delay
  array[N] int<lower=0, upper=1> stop;  // 1 if Stop, 0 if NoStop
}

parameters {
  real alpha;  // intercept
  real beta_rt;  // effect of reaction time
  real beta_ssd;  // effect of stop-signal delay
  vector[P] u;  // participant random effects
  real<lower=0> sigma_u;  // standard deviation of random effects
}

model {
  // Priors
  alpha ~ normal(0, 1);
  beta_rt ~ normal(0, 1);
  beta_ssd ~ normal(0, 1);
  sigma_u ~ cauchy(0, 1);
  u ~ normal(0, sigma_u);

  // Likelihood
  stop ~ bernoulli_logit(alpha + beta_rt * rt + beta_ssd * ssd + u[participant]);
}

generated quantities {
  vector[N] log_lik;
  vector[N] y_pred;

  for (n in 1:N) {
    real p = inv_logit(alpha + beta_rt * rt[n] + beta_ssd * ssd[n] + u[participant[n]]);
    log_lik[n] = bernoulli_logit_lpmf(stop[n] | alpha + beta_rt * rt[n] + beta_ssd * ssd[n] + u[participant[n]]);
    y_pred[n] = bernoulli_rng(p);
  }
}
