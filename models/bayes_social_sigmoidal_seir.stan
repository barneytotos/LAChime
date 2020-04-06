/*
*  Bayesian version of the SEIR model
*  The likelihood assumes that we only observed proportion of the true cases
*/ 
functions {

    /* Computes step-wise SIR predictions
    * @param N:  the number of people in the population
    * @param I0: the number of initially infected people
    * @param beta; the beta parameter
    * @param gamma: the gamma parameter
    */
    vector discrete_seir(int N, real E0, real gamma, real beta, real alpha, int last_time, vector social_distance, int step_size){
    
      vector[last_time] delta;
      
      real S = N;
      real E = E0;
      real I = E0*alpha; 
      real R = 0;
      real NN = S + E + I + R;
      
      real out;
      real delta_exp;
      real delta_inf;
      real delta_rec;
      real scale;
      real ss = 1.0*step_size;
      
      // Run a time updates at time -2, -1, 0 to add to 
      // The infected population
      for (t in -7:last_time){
        
        if (t > 0) delta[t] = 0;
        
        for (s in 1:step_size){
          
          //  Compute the differences
          delta_exp = beta*S*I/N/ss;
          delta_inf = alpha*E/ss;
          delta_rec = gamma*I/ss;

          // Add the distancing
          if (t>0) delta_exp *= social_distance[t];
          if (t>0) delta[t] += delta_exp;
          
          
          // Update the counts
          S -= delta_exp;
          E += delta_exp - delta_inf;
          I += delta_inf - delta_rec;
          R += delta_rec;
          
          // Handle the cases where things dip below 0
          if (S < 0) S=0.0;
          if (E < 0) E=0.0;
          if (I < 0) I=0.0;
          if (R < 0) R=0.0;
          
          // Make sure the total still scales to N
          scale = NN / (S+E+I+R);
          S = S * scale;
          E = E * scale;
          I = I * scale;
          R = R * scale;
          
        }
      }
      return delta;
  }
}


data {
  // Meta data
  int<lower=0> N;                 // size of the maximum population
  int<lower=0> last_time;         // Largest value in times. Should do this in stan but it's a pain
  int<lower=1> intervention_time; // day at which the intervention occurs
  
  int<lower=1> step_size;
  // DPH data
  int<lower=0>                T;         // day at which you observe each DPH data (relative to day0)
  int<lower=0>                lag;           // number of days to lag the observations
  int<lower=lag+1>            times[T];  // day at which each observation occurs
  int<lower=0>                ys[T];     // number of newly observed infected patients at each time
  vector<lower=0, upper=1>[T] ps;            // probability of observing a newly infected person at time t
  
  // Priors
  real<lower=0> exposure_mean;
  real<lower=0> exposure_sd;
  
  real<lower=0> recovery_mean;
  real<lower=0> recovery_sd;
  
  real<lower=0> doubling_mean;
  real<lower=0> doubling_sd;
}

transformed data {
  // Prior re-parameterixations
  real<lower=0> recovery_b = recovery_mean/pow(recovery_sd, 2.0);
  real<lower=0> recovery_a = recovery_mean * recovery_b;
  
  real<lower=0> exposure_b = exposure_mean/pow(exposure_sd, 2.0);
  real<lower=0> exposure_a = exposure_mean * exposure_b;
  
  real<lower=0> doubling_b = doubling_mean/pow(doubling_sd, 2.0);
  real<lower=0> doubling_a = doubling_mean*doubling_b;

  real<lower=0> E0_mean=100;
  real<lower=0> E0_sd=1000;
  
  int<lower=1> lagged_times[T];
  for (t in 1:T) lagged_times[t] = times[t] - lag;
}

parameters {
  real<lower=0> doubling_time;
  real<lower=1> exposure_time;
  real<lower=1> recovery_time;
  real<lower=1, upper=2> sigma;
  real<lower=0> E0;
  real<lower=0, upper=1> social;
  
  real intercept;
  real<lower=0.5, upper=2> slope;
}

transformed parameters{
  
  // SIR parameters
  real<lower=0> alpha = 1.0/exposure_time;
  real<lower=0> gamma = 1.0/recovery_time;
  real<lower=0> beta = pow(2.0, 1.0/doubling_time)-1.0+gamma;
  real<lower=0> phi = 1.0 / (pow(sigma, 2) - 1);

  // Social distancing parameterlength
  vector<lower=0, upper=1>[last_time+90] social_distance;
  vector<lower=0>[last_time] newly_exposed;
  
  // Lame looping to add the social distanceing
  // for (t in 1:(last_time+90)){
  //  if (t <= intervention_time-12) social_distance[t] = 1;
  //  else if (t <= intervention_time + 6 + 12) social_distance[t] = 1 - (1-social) * inv_logit((t-intervention_time-intercept)/slope);
  //  else social_distance[t] = social;
  //}
  for (t in 1:(last_time+90)){
    social_distance[t] = 1 - (1-social) * inv_logit((t-intervention_time-intercept)/slope);
  }
  
  // SIR transformed
  newly_exposed = discrete_seir(N, E0, gamma, beta, alpha, last_time, social_distance, step_size);  
}

model {
  
  // SEIR priors
  doubling_time ~ gamma(doubling_a, doubling_b);
  exposure_time ~ gamma(exposure_a, exposure_b);
  recovery_time ~ gamma(recovery_a, recovery_b);
  E0 ~ normal(E0_mean, E0_sd);
  
  // DPH parameters
  sigma ~ normal(1, 1);
  social ~ beta(1, 1);
  intercept ~ normal(3.5, 5);
  

  // Likelihood
  ys ~ neg_binomial(newly_exposed[lagged_times] .* ps * phi, phi);
}

generated quantities {
  vector<lower=0>[last_time+90] projected_newly_infected;
  vector<lower=0>[last_time+90] sampled_newly_infected;
  projected_newly_infected =  discrete_seir(N, E0, gamma, beta, alpha, last_time+90, social_distance, step_size);
  
  for (t in 1:(last_time+90)) sampled_newly_infected[t] = neg_binomial_rng(projected_newly_infected[t] * phi * ps[1], phi); // this can be wrong...
}
