#' Uses the chime rate/LOS model to forcast total demand
#' @param preds: a single prediction from the sir model
#' @param rate: the demand for the service relative to the total infected population
#' @param length_of_stay: how many days someone stays in the ICU
#' @return vector
forecast_census = function(preds, rate, length_of_stay){
  admits = preds[-1, 4, ] * rate
  admit_in = admits %>% cumsum()
  admit_out = c(rep(0, length_of_stay), head(admits %>% cumsum(), -length_of_stay))
  admit_in - admit_out
}


#' Uses the chime rate/LOS model to forcast new admits to a service
#' @param preds: a single prediction from the sir model
#' @param rate: the demand for the service relative to the total infected population
#' @return vector
forecast_admissions = function(preds, rate){
  preds[-1, 4, ] * rate
}


#' Uses the chime rate/LOS model to forcast new admits to a service
#' New admissions are drawn from a binomial distribution 
#' @param preds: an array of predictions from an sir model
#' @param rate: vector, lenght=# sims, the demand for the service relative to the total infected population
#' @return T x S (Day / Sims) matrix of new admisions 
forecast_admissions_v = function(preds, rate){
  # This is a dumb way to do this
  apply(preds[-1, 4, ], 1, function(x) rbinom(n=length(rate), size=ceiling(x), prob = rate)) %>% t
}


#' Uses the chime rate/LOS model to forcast new admits to a service
#' New admissions are drawn from a negative binomial with dispersion parameterphi
#' @param preds: an array of predictions from an sir model
#' @param rate: vector, lenght=# sims, the demand for the service relative to the total infected population
#' @param phi: vector, negative binomial dispersion parameter for each sim
#' @return T x S (Day / Sims) matrix of new admisions 
forecast_admissions_nb = function(preds, rate, phi){
  # This is a dumb way to do this
  apply(preds[-1, 4, ], 1, function(x) neg_binom_rng(x*rate+1e-6, phi)) %>% t
}

#' Uses the chime rate/LOS model to forcast total demand for multiple simulations
#' @param admits: T x S (Day / Sims) matrix of new admisions, output of forecast_admissions_v
#' @param length_of_stay: how many days someone stays in the ICU
#' @return matrix of TxS matrix (days / sims) showing the current total demand
forecast_census_v = function(admits, length_of_stay){
  admit_in = apply(admits, 2, cumsum)
  zeros = matrix(0, nrow=length_of_stay, ncol = dim(admits)[2])
  
  admit_out = rbind(zeros, head(admit_in, -length_of_stay))
  admit_in - admit_out
}
