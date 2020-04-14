#' Helper function that converts the output
#' Of a preds object and turns it into a longer-dataframe
#' for plotting
#' @param preds: array of dim (times, 2, sims)
#' @return data.frame with columnes t, sim, infected, delta
pred_to_df <- function(preds){
  
  # Get the dimensions
  times = 1:dim(preds)[1]
  sims = dim(preds)[3]
  
  # Convenienve function
  wrapper = function(s) data.frame(t=times, sim=s, infected=preds[,3,s], delta=preds[,4,s])
  map(1:sims, wrapper) %>% bind_rows()
  
}

#' Re-parameterization of the beta rng
#' @param mean: prior mean
#' @param weight: prior number of observations
#' @param sims: how may numbers to draw
sample_beta = function(mean, weight, sims) {
  rbeta(sims, shape1 = mean*weight, shape2=(1-mean)*weight)
}


#' Re-parameterization of the gamma rng
#' @param mean: prior mean
#' @param sd: standard deviation
#' @param sims: how may numbers to draw
sample_gamma = function(mean, sd, sims){
  beta = mean/sd**2
  alpha = mean*beta
  rgamma(sims, shape=alpha, rate=beta)
}



#' Loads ems data, combines it, and reweights by market share
#' @param dur: string, directory where ems data lives
#' @param start_date: first day of data to use (lubridate s3 object)
#' @param stop_date: last day of data to use (lubridate s3 object)
load_ems_data = function(dur='data/ems/', start_date = mdy('3/28/2020'), stop_date = today()){
  
  observed_data = list.files(dur, full.names = TRUE) %>%
    map(read.csv) %>%
    bind_rows() %>%
    filter(
      Category %in% c(
        'total_24est',
        'Confirm_med.surg',
        'Confirm_SDU',
        'Confirm_ICU', 
        'Confirm_vent',
        'total_vent_est',
        'total_estimated',
        'total_ICU_est',
        'total_confirmed'
      )
    ) %>% 
    # Weight by market share
    mutate(
      weighted_value = Value/Marketshare
    ) %>% 
    # Pivot wider
    select(
      Category,
      Date,
      weighted_value
    ) %>%
    tidyr::spread(
      key = 'Category', 
      value = 'weighted_value',
      fill = 0
    ) %>%
    # Combine some of the things
    mutate(
      date = mdy(Date),
      time = as.numeric(date - min(date)) + 1,
      total_non_icu = Confirm_med.surg + Confirm_SDU
    ) %>%
    select(
      date,
      time,
      new_cases = total_24est,
      total_hospital_est = total_estimated,
      total_icu_est = total_ICU_est,
      total_vent_est = total_vent_est,
      total_hospital_confirmed = total_confirmed,
      total_ICU_confirmed = Confirm_ICU,
      total_vent_confirmed = Confirm_vent
    ) %>%
    filter(
      date >= start_date,
      date <= stop_date
    )
}


#' Adds sampling noise to
forecast_new = function(new_cases, phi, proportion){
  prob = phi / (1 + phi) # convert from stan's 'beta' to r's p
  apply(new_cases, 2, function(x) rnbinom(n=10**3, size=x*proportion*phi, prob=prob)) %>% t
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


#' Plots the new admissions/census for a service
#' @param census: samples of (predictive) census
#' @param los: int, length of stay
#' @param future_days: int, number of days to project
#' @param day0: lubridate s3 object indicating the 'day' for index 0
#' @param today: lubridate s3 object indicating today's date
#' @param lag: int, demand lag (used to shift projections)
#' @param color: string, color to plot
#' @return ggplot of the new admissions/census for a single service.
plot_demand = function(census, los, future_days, day0, today, lag=0, name='', color='blue'){
  
  # Set the times scale
  ts = 1:dim(census)[1] + lag
  
  first_day = (today - days(14)) - day0 
  xmin = as.numeric(first_day)
  xmax = as.numeric((today + days(future_days)) - day0)
  x_breaks = seq(xmin, xmax, 7)
  
  # Census uncertainty
  census_df = data.frame(
    ts = ts,
    lower  = apply(census, 1, quantile, probs=0.025),
    middle = apply(census, 1, quantile, probs=0.5),
    upper  = apply(census, 1, quantile, probs=0.975)
  )
  
  # Setup the y axis
  ymax = census_df %>% 
    filter(ts <= xmax) %>% 
    pull(upper) %>% 
    max() %>% 
    smart_max()
  
  # Plot of census
  ggplot()+
    geom_ribbon(
      data = census_df %>% filter(ts<=xmax, ts>=xmin),
      aes(ymin=lower, ymax=upper, x=ts), 
      alpha=0.1,
      fill=color,
      show.legend = FALSE
    ) +
    geom_line(
      data = census_df %>% filter(ts > los) %>% filter(ts<=xmax, ts>=xmin),
      aes(y=middle, x=ts), 
      show.legend = FALSE
    ) +
    geom_vline(
      xintercept = as.numeric(today-day0),
      color = 'dodgerblue',
      linetype = 'dashed'
    ) +
    theme_bw() +
    scale_x_continuous(
      name = NULL,
      limits = c(xmin, xmax),
      breaks = seq(xmin, xmax, 7),
      labels = (day0 + days(x_breaks)) %>% format("%m-%d")
    ) +
    # TODO dynamic scaling
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = seq(0, ymax, length.out = 11),
      name = sprintf('%s', name)
    )  +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
}


smart_max = function(ymax){
  magnitude = floor(log10(ymax*1.2))
  ceiling(ymax*1.2 / 10**magnitude)*10**magnitude
}

