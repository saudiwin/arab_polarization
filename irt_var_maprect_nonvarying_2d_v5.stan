functions {
 vector overT(vector allparams, vector cit,
               real[] time_points, int[] allintdata) {
                 
    // load indices
    int miss = allintdata[1];
    int J = allintdata[2];
    int T = allintdata[3];
    int coup = allintdata[4];
    real t = time_points[1];
    int N = (num_elements(allintdata)-4)/5; // has to be hard-coded unfortunately, no way to 
                                        // pass other info in
    int gen_out_miss[miss] = rep_array(1,miss); // retrieve indices in order
    int gen_out_nomiss[N-miss] = rep_array(0,N-miss);
    int gen_out_obs[N-miss] = allintdata[(miss+5):(N+4)];
    int country_code[N] = allintdata[(N+5):(2*N+4)];
    int tt[N] = allintdata[((2*N)+5):(3*N+4)];
    int jj1[N] = allintdata[((3*N)+5):(4*N+4)];
    int jj2[N] = allintdata[((4*N)+5):(5*N+4)];
    // citizen params 
    
    real delta_11 = cit[1];
    real delta_10 = cit[2];
    real delta_21 = cit[3];
    real delta_20 = cit[4];
    real beta_1 = cit[5];
    real beta_0 = cit[6];
    
    vector[miss] linear_miss;
    vector[N-miss] linear_obs1;
    vector[N-miss] linear_obs2;
    
    vector[1] log_lik;
    
    // separate missing from observed models to speed up
    // with vectorizations
    
  // loop over outcome
  // use hurdle model for missing data (99999)
  // conditional on passing hurdle, use Poisson distribution for retweet counts
  
    
    for(n in 1:miss) {
      real this_alpha1 = allparams[(J*(tt[n]-1))+jj1[n]];
      real this_alpha2 = allparams[(T*J + J*(tt[n]-1))+jj2[n]];
      
      linear_miss[n] = delta_10*this_alpha1  +
                                        delta_20*this_alpha2 - beta_0;
    }
    
    for(n in (miss+1):N) {
      real this_alpha1 = allparams[(J*(tt[n]-1))+jj1[n]];
      real this_alpha2 = allparams[(T*J + J*(tt[n]-1))+jj2[n]];
      
      linear_obs1[n-miss] = delta_10*this_alpha1  +
                                        delta_20*this_alpha2 - beta_0;
      linear_obs2[n-miss] = delta_11*this_alpha1 +
                                  delta_21*this_alpha2 -
                                  beta_1;
    }
    
    log_lik[1] = bernoulli_logit_lpmf(gen_out_miss|linear_miss) + 
           bernoulli_logit_lpmf(gen_out_nomiss|linear_obs1) + 
           poisson_log_lpmf(gen_out_obs|linear_obs2) +
           normal_lpdf(cit|0,3);
                
   return log_lik; // return joint probability of the shard
 }
}
data {
  int<lower=1> J;              // number of elites
  int<lower=1> K;              // number of citizens
  int<lower=1> N;              // number of observations per shard
  int<lower=1> T;  //number of time points
  int<lower=1> C; //number of total data columns
  int<lower=1> S; //number of shards in data for map_rect = number of citizens
  int alldata[S,N]; // data in shard format
  real time_points[S,1]; // counter for citizen IDs
  int coup; //when the coup happens
  vector[T-1] time_gamma;
}
transformed data {
  // calculate how the indices will work for the transformed parameters
  // varying is easy for citizens
  // static must include *all* time-varying parameters as they *cannot* vary across shards
  int vP = 6; // need all citizen parameters discrimination + difficulty for one citizen = 4 parameters
  int dP = 2*T*J; // all alpha parameters plus adjustment/betax/sigmas/country
  //need a vector to pad the time-varying parameters for the case when T=1
  vector[J] padT = rep_vector(0,J);
  real x_r[S,0]; // nothing vector to fil out map_rect 
}
parameters {    
  vector[vP] varparams[S];
  vector<lower=-1,upper=1>[4] adj_in1;  //adjustment parameters
  vector<lower=-1,upper=1>[4] adj_out1; //adjustment parameters
  vector<lower=-1,upper=1>[4] adj_in2;  //adjustment parameters
  vector<lower=-1,upper=1>[4] adj_out2; //adjustment parameters
  vector[4] alpha_int1; //drift
  vector[4] alpha_int2; //drift
  vector[4] betax1; //effects of coup
  vector[4] betax2; //effects of coup
  vector[dP-(2*J)] dparams_nonc; // non-centering time series
  vector<lower=0>[3] sigma_time1; //heteroskedastic variance by ideological group
  vector<lower=0>[3] sigma_time2; //heteroskedastic variance by ideological group
}

transformed parameters {
  
    vector[dP] dparams;
    vector[J] sigma_time1_con;
    vector[J] sigma_time2_con;

    sigma_time1_con = append_row([.1]',sigma_time1);
    sigma_time2_con = append_row([.1]',sigma_time2);
    
  // pack all the citizen parameters into an array vector for usage in map_rect
  
  //all elite params are in non-varying vectors
  
  // append all other parameters to one big vector that is passed to all shards
  // order.
  // pack this vector in a way that allows for time series to 
  // influence each other via heirarchical priors
  
  for(t in 1:T) {
    if(t==1) {
      dparams[1:J] = alpha_int1;
      dparams[(T*J+1):(T*J+J)] = alpha_int2;
    } else {
      for(j in 1:J) {
        int other;
        if(j==1) {
          other = 2;
        } else if(j==2) {
          other=1;
        } else if(j==3) {
          other=4;
        } else if(j==4) {
          other=3;
        }
        dparams[((t-1)*J + j)] = alpha_int1[j] +
                              adj_in1[j]*dparams[((t-2)*J + j)] +
                              adj_out1[j]*dparams[((t-2)*J + other)] +
                              betax1[j]*time_gamma[t-1] +
                              sigma_time1_con[j]*dparams_nonc[((t-1)*J + j)];
        dparams[(T*J + (t-1)*J +j)] = alpha_int2[j] +
                              adj_in2[j]*dparams[((t-2)*J + j + T*J)] +
                              adj_out2[j]*dparams[((t-2)*J + other + T*J)] +
                              betax2[j]*time_gamma[t-1] +
                              sigma_time2_con[j]*dparams_nonc[((t-2)*J + j + (T-1)*J)];
      }
    }
  }
  
}

model {
  
  //pin the intercepts for D2
  
  alpha_int2[1] ~ normal(1,.01);
  alpha_int2[2:4] ~ normal(0,3);
  
  alpha_int1[1] ~ normal(-1,.01);
  alpha_int1[2:4] ~ normal(0,3);
  adj_out1 ~ normal(0,2);
  adj_in2 ~ normal(0,2);
  adj_out2 ~ normal(0,2);
  adj_in1 ~ normal(0,2);
  dparams_nonc ~ normal(0,1); // non-centering time series prior

  sigma_time1 ~ normal(0,1);
  sigma_time2 ~ normal(0,1);
  betax1 ~ normal(0,3);
  betax2 ~ normal(0,3);
  
  // parallelize the likelihood with map_rect
  
  target += sum(map_rect(overT, dparams, varparams, time_points, alldata));

}
generated quantities {
  
  // reconvert varying parameters to square matrices for easy use in R
  matrix[J,T] alpha1_m = to_matrix(dparams[1:(T*J)],J,T);
  matrix[J,T] alpha2_m = to_matrix(dparams[(T*J+1):(2*T*J)],J,T);
}

