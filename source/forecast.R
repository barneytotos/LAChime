#' Uses the chime rate/LOS model to forcast total demand
#' @param preds: a single prediction from the sir model
#' @param rate: the demand for the service relative to the total infected population
#' @param length_of_stay: how many days someone stays in the ICU
#' @return vector
forecast_census = function(preds, rate, length_of_stay){
  
  # This is a dumb way to do this
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
  
  # This is a dumb way to do this
  preds[-1, 4, ] * rate

}


#' Uses the chime rate/LOS model to forcast new admits to a service
#' @param preds: a single prediction from the sir model
#' @param rate: the demand for the service relative to the total infected population
#' @return vector
forecast_admissions_v = function(preds, rate){
  
  # This is a dumb way to do this
  apply(preds[-1, 4, ], 1, function(x) rbinom(n=length(rate), size=ceiling(x), prob = rate)) %>% t
  
}


#' Uses the chime rate/LOS model to forcast total demand
#' @param preds: a single prediction from the sir model
#' @param rate: the demand for the service relative to the total infected population
#' @param length_of_stay: how many days someone stays in the ICU
#' @return vector
forecast_census_v = function(admits, rate, length_of_stay){
  
  # This is a dumb way to do this
  #admits = apply(preds[-1, 4, ], 1, function(x) x*rate) %>% t
  
  admit_in = apply(admits, 2, cumsum)
  zeros = matrix(0, nrow=length_of_stay, ncol = length(rate))
  
  admit_out = rbind(zeros, head(admit_in, -length_of_stay))
  admit_in - admit_out
  
}



plot_forecast = function(model, infection_data, future_days, rate, length_of_stay, detection_probability){

  
  # Do the demand forecasting
  preds = predict(model, future_days+nrow(infection_data), detection_probability=detection_probability, sims=1000)
  out = forecast_rate_los(preds, rate, length_of_stay)
  
  # Make a data frame
  forecast_data = data.frame(
    ts = 2:(future_days+nrow(infection_data))-nrow(infection_data),
    middle = rowMeans(out),
    lower = apply(out, 1, quantile, probs=0.025),
    upper = apply(out, 1, quantile, probs=0.975)
  )
  
  # Make a plot
  ggplot(
      data=forecast_data,
      aes(x=ts)
    ) +
    geom_line(
      aes(y=middle)
    ) +
    geom_ribbon(
      aes(ymin=lower, ymax=upper),
      alpha = 0.1,
      fill ='red'
    ) +
    scale_x_continuous(
      name = 'Days from today',
      limits = c(0, future_days),
      breaks = seq(0, future_days, 7)
    ) +
    scale_y_continuous(
      name = 'Predicted demand'
    ) +
    theme_bw()
  
}
