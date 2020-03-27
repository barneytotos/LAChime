###########################################################
## SIR class 
###########################################################

#' The is the super class for SIR predictions modesl
#' @param N: int, size of the total population
#' @param name: string, used for pretty printing
#' @param I0: the initial number of infected people.
SIR = function(N, name, I0, doubling_time=NULL, recovery_time=NULL, ..., class = character()){
  
  stopifnot(I0 > 0)
  stopifnot(N > 0)
  
  # initialize the S3 class
  structure(
    list(
      N = N,
      name = name,
      I0 = I0,
      doubling_time = doubling_time,
      recovery_time = recovery_time,
      ...
    ), 
    class = c(class, "SIR")
  )
}


###########################################################
## Generics for the SIR class
###########################################################

#' Fits the model to a set of data parameters
#' @param model: SIR model instance
#' @param ...: additional arguments to be passed for the model fitting
#' @return SIR model fit to the data
fit = function(model, ...) UseMethod("fit")  

fit.SIR = function(model, ...) {
  warning(sprintf("Fit method not implemented for model (name=%s)", model$name))
  return(model)   
}


#' Predicts the number of S / I / R at times points 
#' @param model: SIR model instance
#' @param last_time: int, the last time period to predict
#' @param social_reduction: function; returns the reduction related to interventions in period t . 
#' @param sims:  int, number of simulations to perform
#' @return T x 2 x sims matrix of I / delta_I values
predict.SIR = function(model, last_time, social_reduction = function(x) {1}, sims=1, ...) {
  stopifnot(0 < last_time)
  stop(sprintf("Predict method not implemented for model (name=%s)", model$name))
}


#' Plots the number of infected at time points (future/past)
#' @param model: SIR model instance
#' @param preds: array of dim (times, 2, sims); output from the predict method
#' @return ggplot showing the # of infected at each time point. Or maybe something else
plot.SIR = function(model, preds, times=NA, infected=NA, ...) {
  
  plot_data = pred_to_df(preds)
  sims = dim(preds)[3]

  # Plot the currently infected
  g1 = ggplot() +
    geom_line(
      data=plot_data,
      aes(x=t, group=sim, y=infected/model$N, color = 'total'),
      alpha = sqrt(1/sims)
    ) +
    geom_line(
      data=plot_data,
      aes(x=t, group=sim, y=delta/model$N, color = 'new'),
      alpha = sqrt(1/sims)
    ) +
    scale_y_continuous(
      name='Proportion of population currently infected',
      limits=c(-0.001, 0.1),
      breaks=seq(0, 0.08, 0.02),
      expand=c(0,0)
    ) +
    scale_x_continuous(
      name='Time',
      expand=c(0,0)
    ) +
    scale_color_manual(
      values = c('total'='black', 'new'='red', 'observed'='orchid'),
      labels = c('total'='Predicted Total', 'new'='Predicted New', 'observed'='Observed')
    ) +
    theme_bw() + 
    theme(
      legend.position = c(0.01, 0.99), 
      legend.justification = c(0, 1),
      legend.key.size = unit(1, "line"),
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.8)),
      legend.background = element_rect(fill = NA),
      axis.text.y = element_text(angle = 90, hjust = 0.5)
    ) 
  
  # Add data if it exists.
  if (!any(is.na(times))){
    
    obs_data = data.frame(
      t = times,
      y = infected
    )
    
    g1 = g1 + geom_line(
        data=obs_data,
        aes(x=t, y=infected/model$N, color='observed'),
        size=2
      )
  }
  
  g1 
  
  
}


###########################################################
## Basic SIR class 
###########################################################

#' The is the most basic SIR model (as implemented in CHIME)
#' Uses a discete process to approximate the SIR model
#' Describes the progression 
#' @param N: int, size of the total population
#' @param I0: the initial number of infected people.
#' @param doubling_time: time it takes for the infections population to 
#'        double (early on in the disease). T_d in the CHIME model
#' @param recovery_time: average number of days it takes to recover
BasicSIR = function(N, I0, doubling_time, recovery_time){
  
  # Bad error handling
  stopifnot(doubling_time>0)
  stopifnot(recovery_time>0)

  # initialize the S3 class
  SIR(
    N=N,
    name='basic',
    I0=I0,
    doubling_time=doubling_time,
    recovery_time=recovery_time,
    class="BasicSIR"
  )
}


#' Predicts the number of Infected / change in infected at each time points 
#' @param model: SIR model instance
#' @param last_time: int, the last time period to predict
#' @return T x 2 x sims matrix of I / delta_I values
predict.BasicSIR = function(model, last_time, contact_reduction=0) {

  #' Initialize the populations at time 
  S = model$N # - model$I0
  I = model$I0
  R = 0
  NN = S+I+R
  
  #' Compute the key parameters
  gamma = 1 / model$recovery_time
  growth_rate = 2**(1/model$doubling_time)-1
  beta = (growth_rate + gamma) * (1-contact_reduction) / model$N

  #' Intialize the storage
  preds = array(dim=c(last_time+1, 4, 1))
  
  preds[1, 1, 1] = S
  preds[1, 2, 1] = I
  preds[1, 3, 1] = R
  preds[1, 4, 1] = 0

  # Do the numerical integration
  for (t in 1:last_time){

    delta_in = beta*S*I
    delta_out = gamma*I

    # Update the counts5
    S = S - delta_in
    I = I + delta_in - delta_out
    R = R + delta_out
    
    # Handle the cases where things dip below 0
    S[S<0] = 0
    I[I<0] = 0
    R[R<0] = 0
    
    # Make sure the still scale to N
    scale = NN / (S+I+R)
    S = S * scale
    I = I * scale
    R = R * scale
    
    preds[t+1, 1, 1] = S
    preds[t+1, 2, 1] = I
    preds[t+1, 3, 1] = R
    preds[t+1, 4, 1] = delta_in
  }
  
  colnames(preds) = c('S', 'I', 'R', 'new')
  return(preds)
}


###########################################################
## Stochastic SIR class 
###########################################################

#' The is the basic SIR model but the parameters are may be randomly sampled from distributions.
#' In addition, the observed infections are modeled as binomial noise: I_obs ~ Bin(I, p) 
#' Uses a discete process to approximate the SIR model
#' @param N: int, size of the total population
#' @param I0: the initial number of infected people.
#' @param doubling_time: function that returns s doubling times
#' @param recovery_time: function that returns s recovery times
StochasticSIR = function(N, I0, doubling_time, recovery_time){
  
  # initialize the S3 class
  SIR(
    N,
    name='stochastic',
    I0,
    doubling_time,
    recovery_time,
    class="StochasticSIR"
  )
}


#' Predicts the number of Infected / change in infected at each time points 
#' @param model: SIR model instance
#' @param last_time: int, the last time period to predict
#' @param social_reduction: function; returns the reduction related to interventions in period t . 
#' @param detection_probability: real in 0, 1 indicating the proportion of infected patients that are observed
#' @param seed: used to control the randomness in the output
#' @return T x 2 x sims matrix of I / delta_I values
predict.StochasticSIR = function(model, last_time, contact_reduction, seed=sample.int(.Machine$integer.max, 1)) {
  
  set.seed(seed)
  
  #' Initialize the populations at time 0
  S = model$N 
  I = model$I0
  R = 0
  NN = S + I + R
  
  #' Compute the key parameters
  gamma = 1 / model$recovery_time
  growth_rate = 2**(1/model$doubling_time)-1
  beta = (growth_rate + gamma) * (1-contact_reduction) / model$N
  
  #' Intialize the storage
  preds = array(dim=c(last_time+1, 4, length(beta)))
  
  preds[1, 1, ] = S
  preds[1, 2, ] = I
  preds[1, 3, ] = R
  preds[1, 4, ] = 0
  
  # Do the numerical integration
  for (t in 1:last_time){
    
    # Compute the differences
    delta_in = beta*S*I
    delta_out = gamma*I
    
    # Update the counts
    S = S - delta_in
    I = I + delta_in - delta_out
    R = R + delta_out
    
    # Some error handling that presumably comes 
    # From the discretezation
    
    # Handle the cases where things dip below 0
    S[S<0]=0
    I[I<0]=0
    R[R<0]=0
    
    # Make sure the still scale to N
    scale = NN / (S+I+R)
    S = S * scale
    I = I * scale
    R = R * scale
    
    # Store the stuff
    preds[t+1, 1, ] = S
    preds[t+1, 2, ] = I
    preds[t+1, 3, ] = R
    preds[t+1, 4, ] = delta_in

  }
  
  return(preds)
}



###########################################################
## Bayes SIR class 
###########################################################
#' The is the basic SIR model but the parameters are may be randomly sampled from distributions.
#' In addition, the observed infections are modeled as binomial noise: I_obs ~ Bin(I, p) 
#' Uses a discete process to approximate the SIR model
#' @param N: int, size of the total population
#' @param I0: the initial number of infected people.
BayesSIR = function(N, I0, stan_fname='models/basic_sir.stan'){
  
  # initialize the S3 class
  SIR(
    N,
    I0,
    name='bayes',
    SM = stan_model(stan_fname, auto_write = TRUE),
    fit=NULL,
    class="BayesSIR"
  )
}


# Fit function for the bayes approach
fit.BayesSIR = function(model, times, infected, p=0.25) {

  # Get the stan data
  stan_data = list(
    T = length(times),
    N = model$N,
    times = times,
    infected = round(infected),
    last_time = max(times),
    p = p
  )
  
  model$fit = sampling(
    model$SM, 
    stan_data,
    iter=2*10**3,
    warmup=10**3,
    chains=1,
    cores=1,
    control = list('adapt_delta'=0.9),
    init = list(list('recovery_time'=14, 'doubling_time'=6, 'I0'=min(infected)))
  )
  
  return(model)
  
}


#' Predicts the number of Infected / change in infected at each time points 
#' @param model: SIR model instance
#' @param last_time: int, the last time period to predict
#' @param social_reduction: function; returns the reduction related to interventions in period t . 
#' @param detection_probability: real in 0, 1 indicating the proportion of infected patients that are observed
#' @param seed: used to control the randomness in the output
#' @return T x 2 x sims matrix of I / delta_I values
predict.BayesSIR = function(model, last_time, social_reduction = function(x) {1}, 
                            detection_probability=1, sims, seed=sample.int(.Machine$integer.max, 1)) {
  
  set.seed(seed)
  
  #' Initialize the populations at time 0
  inds = sample(1000, sims, replace = FALSE)
  
  I0 = extract(model$fit)$I0[inds]
  S = model$N - I0
  I = I0
  R = 0
  
  #' Compute the key parameters
  gamma = extract(model$fit)$gamma[inds]
  beta = extract(model$fit)$beta[inds] 
    
  #' Intialize the storage
  preds = array(dim=c(last_time, 2, sims))
  
  # Do the numerical integration
  for (t in 1:last_time){
    
    # Compute the differences
    delta_in = beta*S*I/model$N*social_reduction(t)
    delta_out = gamma*I
    
    # Update the counts
    S = S - delta_in
    I = I + delta_in - delta_out
    R = R + delta_out
    
    # Some error handling that presumably comes 
    # From the discretezation
    
    # Handle the cases where things dip below 0
    S[S<0]=0
    I[I<0]=0
    R[R<0]=0
    
    # Make sure the still scale to N
    scale = model$N / (S+I+R)
    S = S * scale
    I = I * scale
    R = R * scale
    
    # Add the noise
    props =  detection_probability
    preds[t, 1, ] = I * props
    preds[t, 2, ] = delta_in * props
    
  }
  
  return(preds)
}






