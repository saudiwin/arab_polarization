data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations
  int<lower=1> T;  //number of time points
  int<lower=1,upper=J> jj[N];  // elite ID for observation n
  int<lower=1,upper=K> kk[N];  // citizen ID for observation n
  int<lower=1> tt[N]; // t for observation N
  real y[N];   // outcome for observation n
  vector[T-1] time_gamma; //binary vector indicating when coup happens
  int country_code[N]; //indicator for Tunisia
}
parameters {    
  vector[K] delta_1;                  // non-zero discriminations
  vector[K] delta_0;                  // zero discriminations
  matrix[T,J] alpha;               // time-varying ideal points for elites
  vector[K] beta_0;                // zero difficulty of question k
  vector[K] beta_1;                // zero difficulty of question k
  vector[4] adj_in;  //adjustment parameters
  vector[4] adj_out; //adjustment parameters
  vector[4] alpha_int; //drift
  vector[4] betax; //effects of coup
  real<lower=0> country; //dummy for country-level fixed effects
  vector<lower=0>[4] sigma_time; //heteroskedastic variance by ideological group
  real<lower=0> sigma_beta_0; //hierarchical variance for zero betas
  real<lower=0> sigma_beta_1; //hierarchical variance for zero betas
  real<lower=0> sigma_overall; //variance for top-level normal distribution
}

transformed parameters {
  //constrain one intercept for identification 
  //vector[4] alpha_int_full;
  //alpha_int_full = append_row(alpha_int,-sum(alpha_int));
}

model {
  to_vector(alpha[1,]) ~ normal(0,3);
  alpha_int ~ normal(0,3);
  adj_in ~ normal(0,3);
  adj_out ~ normal(0,3);
  country ~ exponential(.1);
  sigma_time ~ exponential(.1);
  sigma_overall ~ exponential(.1);
  sigma_beta_0 ~ exponential(.1);
  sigma_beta_1 ~ exponential(.1);
  
  //VAR priors with constraints on who influences who
  
  alpha[2:T,1] ~ normal(alpha_int[1] + adj_in[1]*alpha[1:(T-1),1] + 
                        adj_out[1]*alpha[1:(T-1),2] +
                        betax[1]*time_gamma,
                sigma_time[1]);

  alpha[2:T,2] ~ normal(alpha_int[2] + adj_in[2]*alpha[1:(T-1),2] + 
                        adj_out[2]*alpha[1:(T-1),1] +
                        betax[2]*time_gamma,
                sigma_time[2]);
  alpha[2:T,3] ~ normal(alpha_int[3] + adj_in[3]*alpha[1:(T-1),3] + 
                        adj_out[3]*alpha[1:(T-1),4] +
                        betax[3]*time_gamma,
                sigma_time[3]);

  alpha[2:T,4] ~ normal(alpha_int[4] + adj_in[4]*alpha[1:(T-1),4] + 
                        adj_out[4]*alpha[1:(T-1),3] +
                        betax[4]*time_gamma,
                sigma_time[4]);

//citizen priors
  beta_0 ~ normal(0,sigma_beta_0);
  beta_1 ~ normal(0,sigma_beta_1);
  delta_1 ~ normal(0,3);   
  delta_0 ~ normal(0,3);
  // loop over outcome
  // use hurdle model for missing data (-9999)
  // conditional on passing hurdle, use normal distribution IRT model with time-varying
  // parameters for elites
  
    for(n in 1:N) {
      if(y[n]==-9999) {
        1 ~ bernoulli_logit(delta_0[kk[n]]*(alpha[tt[n],jj[n]] + country*country_code[n])  - beta_0[kk[n]]);
      } else {
        0 ~ bernoulli_logit(delta_0[kk[n]]*(alpha[tt[n],jj[n]] + country*country_code[n]) - beta_0[kk[n]]); 
        y[n] ~ normal(delta_1[kk[n]]*(alpha[tt[n],jj[n]] + country*country_code[n]) -
                       beta_1[kk[n]], 
                      1);
      }
    }
}

generated quantities {
  matrix[T,J] alpha_country; //recalculate alpha with country intercepts included  

  alpha_country[,1] = alpha[,1];
  alpha_country[,2] = alpha[,2] + country;
  alpha_country[,3] = alpha[,3];
  alpha_country[,4] = alpha[,4] + country;
}
