#----------------------------
# Forescasting for the sir model
#--------------------------

source('source/setup.R')
source('source/forecast.R')
source('source/plots.R')
library('rstan')
library('patchwork')
library('lubridate')

# Global parameters
input = list(
  lag = 13,
  p = 0.05
)

# Load the frankenstein data
new_case_data = read.csv('data/franken_data.csv') %>%
  mutate(
    date = mdy(date),
    lag_date = date - days(input$lag)
  ) 

# Add time
day0 =  min(new_case_data$date) - days(input$lag+1)
new_case_data = new_case_data %>% mutate(time = as.numeric(date - day0))


# Load the stan model
SM = stan_model(
  'models/bayes_social_sigmoidal_seir.stan', 
  auto_write = TRUE
)

# Setup the stan data
stan_data = list(
  # Meta data
  N = 10**7,
  last_time = max(new_case_data$time),
  intervention_time = as.numeric((mdy('3/12/2020') - day0)) +4,
  # DPH data
  T = nrow(new_case_data),
  lag = input$lag,
  times = new_case_data$time,
  ys = new_case_data$new_cases %>% round(),
  ps = rep(input$p, nrow(new_case_data)),
  # Priors
  exposure_mean = 2.5,
  exposure_sd = 1.0,
  recovery_mean = 10.0,
  recovery_sd = 2.55,
  doubling_mean = 4.5,
  doubling_sd = 1,
  step_size = 12
)

# Model initilization
inits = function(chain_id) {
  list(
    'recovery_time'=10.0, 
    'doubling_time'=3, 
    'exposure_time'=2.5,
    'sigma'=1.5,
    'E0'=10,
    'sigma'=3,
    'social'=0.1,
    'intercept'=3.5,
    'slope'=1.0
  )
}

# Run the model
fit = sampling(
  SM,
  stan_data,
  iter = 10**3 + 10**3,
  warmup = 10**3,
  thin = 1,
  chains = 1,
  cores = 1,
  control = list('adapt_delta'=0.99),
  init = inits
)

samples = rstan::extract(fit)
new_cases = samples$projected_newly_infected

#-------------------------------------
# Things required for forecasting
observed_data = read.csv(
    'data/ems_clean_2020-04-05.csv'
  ) %>%
  mutate(
    date = ymd(date),
    lage_date = ymd(lag_date)
  )

forecast_new = function(new_cases, phi, proportion){
  prob = phi / (1 + phi) # convert from stan's 'beta' to r's p
  apply(new_cases, 2, function(x) rnbinom(n=10**3, size=x*proportion*phi, prob=prob)) %>% t
}
FUTURE = 28

#--------------------------------------
# Project for non-ICU patients
#--------------------------------------
p_non_icu = 0.017
non_icu_los = 7

# Predict admissions
non_icu_admissions = forecast_new(
  new_cases, 
  samples$phi,
  rep(p_non_icu, 1000)
)

plot_demand(
    non_icu_admissions, 
    los = non_icu_los, 
    future_days = FUTURE, 
    day0 = day0, 
    lag = 15,
    name='census of non-ICU confirmed COVID patients', 
    color='blue'
  ) + 
  geom_point(
    data = observed_data,
    aes(x=date-day0, y=total_non_icu)
  ) +
  ggtitle('Census of non-ICU patients over time')
ggsave('presentations/07APR20/non_icu_forecast.png', height = 5, width = 7)

#--------------------------------------
# Project for ICU patients
#--------------------------------------
p_icu = 0.01
icu_los = 9

# Predict admissions
icu_admissions = forecast_new(
  new_cases, 
  samples$phi,
  rep(p_icu, 1000)
)

plot_demand(
    icu_admissions, 
    los = icu_los, 
    future_days = FUTURE, 
    day0 = day0, 
    lag = 15,
    name='census of ICU confirmed COVID patients', 
    color='firebrick'
  ) + 
  geom_point(
    data = observed_data,
    aes(x=date-day0, y=total_icu)
  ) +
  ggtitle('Census of ICU patients over time')
ggsave('presentations/07APR20/icu_forecast.png', height = 5, width = 7)


#--------------------------------------
# Project for non-ICU patients
#--------------------------------------
p_vent = 0.009
vent_los = 10

# Predict admissions
vent_admissions = forecast_new(
  new_cases, 
  samples$phi,
  rep(p_vent, 1000)
)

plot_demand(
    vent_admissions, 
    los = vent_los, 
    future_days = FUTURE, 
    day0 = day0, 
    lag = 15,
    name='census of ventilator confirmed COVID patients', 
    color='goldenrod'
  ) +
  geom_point(
    data = observed_data,
    aes(x=date-day0, y=total_vent)
  ) +
  ggtitle('Census of ventilator patients over time')
ggsave('presentations/07APR20/vent_forecast.png', height = 5, width = 7)

