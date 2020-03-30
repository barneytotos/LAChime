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
    vector discrete_seir(int N, real E0, real gamma, real beta, real alpha, int last_time){
    
      vector[last_time] delta;
      
      real S = N;
      real E = E0;
      real I = 0; 
      real R = 0;
      real NN = N + E0;
      
      real delta_exp;
      real delta_inf;
      real delta_rec;
      real scale;
      
      // Do the updates
      // Run a time updates at time -2, -1, 0 to add to 
      // The infected population
      for (t in 1:last_time){
        
        //  Compute the differences
        delta_exp = beta*S*I/N;
        delta_inf = alpha*E;
        delta_rec = gamma*I;
        
        // Update the counts
        S = S - delta_exp;
        E = E + delta_exp - delta_inf;
        I = I + delta_inf - delta_rec;
        R = R + delta_rec;
        
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
        
        if (t > 0) delta[t] = delta_inf;
        
      }
      return delta;
  }
  
  /* Version of binomial_lpmf that accepts N as a real
  * Probabily not exactly right but whatever
  * todo: convert this to an lpmf
  */
  vector binom_loglik(vector y, vector N, vector p){
    return lgamma(N+1) - lgamma(y+1) - lgamma(N-y+1) + y .* log(p) + (N-y) .* log(1-p);
  }
  
  /* Version of binomial_lpmf that accepts N as a real
  * Probabily not exactly right but whatever
  * todo: convert this to an lpmf
  */
  
  int[] neg_binom_rng(vector mu, vector phi){
    return neg_binomial_2_rng(mu, phi);
  }
  
}


data {
  // Meta data
  int<lower=0> T;            // number of observations
  int<lower=0> N;            // size of the maximum population
  int<lower=0> last_time;          // Largest value in times. Should do this in stan but it's a pain
  
  // Data data
  int<lower=0> times[T];           // time at which each observation occurs
  // vector<lower=0>[T] ys;           // number of newly observed infected patients at each time
  int<lower=0> ys[T];           // number of newly observed infected patients at each time
  vector<lower=0, upper=1>[T] ps;  // probability of observing a newly infected person at time t
  
  // Priors
  real<lower=0> exposure_mean;
  real<lower=0> exposure_sd;
  
  real<lower=0> recovery_mean;
  real<lower=0> recovery_sd;
  
  real<lower=0> doubling_mean;
  real<lower=0> doubling_sd;
  

}

transformed data {
  
  real<lower=0> recovery_b = recovery_mean/pow(recovery_sd, 2.0);
  real<lower=0> recovery_a = recovery_mean * recovery_b;
  
  real<lower=0> exposure_b = exposure_mean/pow(exposure_sd, 2.0);
  real<lower=0> exposure_a = exposure_mean * exposure_b;
  
  real<lower=0> doubling_b = doubling_mean/pow(doubling_sd, 2.0);
  real<lower=0> doubling_a = doubling_mean*doubling_b;

  real<lower=0> E0_mean=100;
  real<lower=0> E0_sd=1000;
  
}

parameters {
  real<lower=0> doubling_time;
  real<lower=0> exposure_time;
  real<lower=0> recovery_time;
  real<lower=0> phi;
  real<lower=0> E0;
}

transformed parameters{
  
  // SIR parameters
  real<lower=0> alpha = 1.0/exposure_time;
  real<lower=0> gamma = 1.0/recovery_time;
  real<lower=0> beta = pow(2.0, 1.0/doubling_time)-1.0+gamma;
  
  // Likelihood parameters
  vector<lower=0>[last_time] newly_exposed = discrete_seir(N, E0, gamma, beta, alpha, last_time);   
  
}

model {
  
  // Priors
  doubling_time ~ gamma(doubling_a, doubling_b);
  exposure_time ~ gamma(exposure_a, exposure_b);
  recovery_time ~ gamma(recovery_a, recovery_b);
  E0 ~ normal(E0_mean, E0_sd);

  // Likelihood
  ys ~ neg_binomial_2(newly_exposed[times] .* ps, phi);

}
