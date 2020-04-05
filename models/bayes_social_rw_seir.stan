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
    vector discrete_seir(int N, real E0, real gamma, real beta, real alpha, int last_time, vector social_distance){
    
      vector[last_time] delta;
      
      real S = N;
      real E = E0;
      real I = E0*alpha; 
      real R = 0;
      real NN = S + E + I + R;
      
      real delta_exp;
      real delta_inf;
      real delta_rec;
      real scale;
      
      // Run a time updates at time -2, -1, 0 to add to 
      // The infected population
      for (t in -7:last_time){
        
        //  Compute the differences
        delta_exp = beta*S*I/N;
        delta_inf = alpha*E;
        delta_rec = gamma*I;
        
        // Add the distancing
        if (t>0) delta_exp *= social_distance[t];
        
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
        
        if (t > 0) delta[t] = delta_exp;
        
      }
      return delta;
  }
}


data {
  // Meta data
  int<lower=0> N;                 // size of the maximum population
  int<lower=0> last_time;         // Largest value in times. Should do this in stan but it's a pain
  int<lower=1> intervention_time; // day at which the intervention occurs
  
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
  real<lower=0> tau;
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
  
  int<lower=1> n_social = last_time - intervention_time + 1 + 7;  // The 7 is random; don't want to RW forever
  int<lower=1> lagged_times[T];
  
  for (t in 1:T) lagged_times[t] = times[t] - lag;
}

parameters {
  real<lower=0> doubling_time;
  real<lower=1> exposure_time;
  real<lower=1> recovery_time;
  real<lower=1, upper=2> sigma;
  real<lower=0> E0;
  ordered[n_social] theta;
  real<lower=0> eta;
}

transformed parameters{
  
  // SIR parameters
  real<lower=0> alpha = 1.0/exposure_time;
  real<lower=0> gamma = 1.0/recovery_time;
  real<lower=0> beta = pow(2.0, 1.0/doubling_time)-1.0+gamma;
  real<lower=0> phi = 1.0 / (pow(sigma, 2) - 1);

  // Social distancing parameterlength
  vector<lower=0, upper=1>[n_social] social; 
  vector<lower=0, upper=1>[last_time+90] social_distance;
  vector<lower=0>[last_time] newly_exposed;
  
  // Lame looping to add the social distanceing
  social = inv_logit(-theta);
  for (t in 1:(last_time+90)){
    if (t <= intervention_time) social_distance[t] = 1;
    else if (t <= n_social) social_distance[t] = social[t-intervention_time];
    else social_distance[t] = social[n_social-intervention_time];
  }
  
  // SIR transformed
  newly_exposed = discrete_seir(N, E0, gamma, beta, alpha, last_time, social_distance);  
}

model {
  
  // SEIR priors
  doubling_time ~ gamma(doubling_a, doubling_b);
  exposure_time ~ gamma(exposure_a, exposure_b);
  recovery_time ~ gamma(recovery_a, recovery_b);
  E0 ~ normal(E0_mean, E0_sd);
  
  // DPH parameters
  sigma ~ normal(1, 1);
  
  // Beta random walk
  eta ~ student_t(1, 0, tau);
  theta[1] ~ normal(-3, tau);
  for (t in 2:n_social) {
    theta[t] ~ normal(theta[t-1], tau);
  }
  
  // Likelihood
  ys ~ neg_binomial(newly_exposed[lagged_times] .* ps * phi, phi);
}

generated quantities {
  vector<lower=0>[last_time+90] projected_newly_infected;
  projected_newly_infected =  discrete_seir(N, E0, gamma, beta, alpha, last_time+90, social_distance);
}
