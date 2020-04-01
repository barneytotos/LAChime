/*
*  Bayesian version of the basic SIR model
*  The likelihood assumes that we only observed proportion of the true cases
*/ 
functions {
    
    /* Computes step-wise SIR predictions
    * @param N:  the number of people in the population
    * @param I0: the number of initially infected people
    * @param beta; the beta parameter
    * @param gamma: the gamma parameter
    */
    vector discrete_sir(int N, real I0, real gamma, real beta, int last_time){
    
      vector[last_time] delta;
      
      real S = N;
      real I = I0;
      real R = 0;
      real NN = N + I0;
      
      real delta_in;
      real delta_out;
      real scale;
      
      // Do the updates
      for (t in 1:last_time){
        
        //  Compute the differences
        delta_in = beta*S*I/N;
        delta_out = gamma*I;
        
        // Update the counts
        S = S - delta_in;
        I = I + delta_in - delta_out;
        R = R + delta_out;
        
        // Handle the cases where things dip below 0
        if (S < 0) S=0.0;
        if (I < 0) S=0.0;
        if (R < 0) S=0.0;
        
        // Make sure the total still scales to N
        scale = NN / (S+I+R);
        S = S * scale;
        I = I * scale;
        R = R * scale;
        
        delta[t] = delta_in;
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
  real<lower=0> I0_mean;
  real<lower=0> I0_sd;
  
  real<lower=0> recovery_mean;
  real<lower=0> recovery_sd;
  
  real<lower=0> doubling_mean;
  real<lower=0> doubling_sd;
  

}

transformed data {
  
  real<lower=0> recovery_b = recovery_mean/pow(recovery_sd,2.0);
  real<lower=0> recovery_a = recovery_mean * recovery_b;
  
  
  real<lower=0> doubling_b = doubling_mean/pow(doubling_sd, 2.0);
  real<lower=0> doubling_a = doubling_mean*doubling_b;
  
}

parameters {
  real<lower=0> doubling_time;
  real<lower=0> recovery_time;
  real<lower=0> I0;
  real<lower=0> phi;
}

transformed parameters{
  
  // SIR parameters
  real<lower=0> gamma = 1.0/recovery_time;
  real<lower=0> beta = pow(2.0, 1.0/doubling_time)-1.0+gamma;
  
  // Likelihood parameters
  vector<lower=0>[last_time] newly_infected = discrete_sir(N, I0, gamma, beta, last_time);   
  
}

model {
  
  // Priors
  doubling_time ~ gamma(doubling_a, doubling_b);
  recovery_time ~ gamma(recovery_a, recovery_b);
  I0 ~ normal(I0_mean, I0_sd);

  // Likelihood
  // target += binom_loglik(ys, newly_infected[times], ps);
  ys ~ neg_binomial_2(newly_infected[times] .* ps, phi);
  target += -sqrt(phi);

}
