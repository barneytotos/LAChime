#--------------------------------------------------
# Demo of a Bayesian SEIR model using LAX county PH data
# Model is implemented with lags and social distancing
# This aligns reasonably closesly with the bChime app
# -------------------------------------------------

source('source/setup.R')
source('source/forecast.R')
source('source/models.R')
source('source/utils.R')
source('source/plots.R')


# This comes from the IO
input = list(
  # Data
  p = 0.15,
  lag = 10,
  
  # Priors
  exposure_mean = 2.5,
  exposure_sd = 1.0,
  recovery_mean = 9.0,
  recovery_sd = 2.55,
  doubling_mean = 4.5,
  doubling_sd = 0.5,
  
  # Social distancing
  social = 0,
  
  # Forecasting parameterize
  hospital_los=7,
  icu_los=9,
  ventilator_los=10,
  hospital_relative_rate=0.1,
  icu_relative_rate=0.03,
  ventilator_relative_rate=0.02,
  
  # Other things
  future_days = 42
)

# Load the data
df <- readr::read_csv('data/lax_county_data_26MAR20.csv') %>% 
  tail(10) %>%
  mutate(
    date = mdy(day),
    time = as.numeric(date - min(date)) + 1,
    lag_date = date - days(input$lag),
    lag_time = as.numeric(lag_date - min(lag_date)) + 1
  )

#--------------------------------------------------
# important time parameters
# -------------------------------------------------
day0 = min(df$lag_date) - days(1)
start_social = mdy('3/12/2020')
full_social = mdy('3/19/2020')
demand_lag = 13

#--------------------------------------------------
# Fit the model
# -------------------------------------------------

# Initialize
model = BayesSEIR(
  10**7,
  exposure_mean=input$exposure_mean,
  exposure_sd=input$exposure_sd,
  recovery_mean=input$recovery_mean,
  recovery_sd=input$recovery_sd,
  doubling_mean=input$doubling_mean,
  doubling_sd=input$doubling_sd
)

# Fit the model
model = fit(
  model, 
  df$lag_time, 
  df$new_cases,
  p = 0.1
)


print(model$fit, pars =c('E0', 'exposure_time', 'recovery_time', 'doubling_time', 'sigma'))

# This is the social distancing function
# It shifts results to the left.
# I plan on implementing a better version in the future (that lags the results at the modeling stage)

social = 1 - input$social
contact_reduction = function(x) case_when(
  x <= (start_social - day0) ~ 1,
  x <= (full_social - day0)  ~ 1 - (1-social) * (x-as.numeric(start_social - day0)) / 7,
  TRUE ~ social
)


# Do the predictions
preds = predict(
  model, 
  last_time = as.numeric(today() - day0 + 180),
  contact_reduction=contact_reduction
)


# Predict admissions
new_cases = forecast_new(
  model,
  preds, 
  rep(1, 1000)
)

# Predict admissions
hospital_admissions = forecast_new(
  model,
  preds, 
  rep(input$hospital_relative_rate, 1000)
)

# Predict admissions
icu_admissions = forecast_new(
  model,
  preds, 
  rep(input$icu_relative_rate*input$hospital_relative_rate, 1000)
)

# Predict admissions
ventilator_admissions = forecast_new(
  model,
  preds, 
  rep(input$ventilator_relative_rate*input$hospital_relative_rate, 1000)
)




#--------------------------------------------------
#  Make nice plots
# -------------------------------------------------
g = plot_demand(
    new_cases, 
    los = input$hospital_los, 
    future_days = input$future_days, 
    day0 = day0, 
    lag = demand_lag,
    name='hospital', 
    color='blue'
  ) + ggtitle('Current total hospital admissions')

g
#--------------------------------------------------
#  Make nice plots
# -------------------------------------------------
g1 = plot_demand(
    hospital_admissions, 
    los = input$hospital_los, 
    future_days = input$future_days, 
    day0 = day0, 
    lag = demand_lag,
    name='hospital', 
    color='blue'
  ) + ggtitle('Current total hospital admissions')


# Plot new admissions
g2 = plot_demand(
    icu_admissions, 
    los = input$icu_los, 
    future_days = input$future_days, 
    day0 = day0, 
    lag = demand_lag,
    name='ICU', 
    color='firebrick'
  ) + ggtitle('Current total ICU patients')

# Plot new admissions
g3 = plot_demand(
    ventilator_admissions, 
    los = input$ventilator_los, 
    future_days = input$future_days,
    day0 = day0, 
    lag = demand_lag,
    name='ventilator', 
    color='goldenrod'
  ) + ggtitle('Current total ventilator patients')

g1 | g2 | g3

