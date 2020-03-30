#--------------------------------------------------
# This file contains a stochastic version of the chime model
# -------------------------------------------------
source('source/setup.R')
source('source/models.R')
source('source/utils.R')
source('source/forecast.R')

# Mimic the ui I plan to implement in shiny
input = list(
  # Fixed parameters
  N=10**6,
  I=14,
  market_share=0.8,
  # Prior parameters
  doubling_time_mean=5,
  doubling_time_sd=1,
  recovery_time_mean=14,
  recovery_time_sd=1,
  hospitalization_mean=0.1,
  hospitalization_weight=10,
  contact_reduction_mean=0.30,
  contact_reduction_weight=10,
  # Forecasting parameterize
  hospital_los=7,
  icu_los=9,
  ventilator_los=10,
  icu_relative_rate=0.3,
  ventilator_relative_rate=0.2,
  # Prediction parameters
  future_days=60,
  sims=100
)


#--------------------------------------------------
# Sample all of the random paramters
# -------------------------------------------------
doubling_time = sample_gamma(
  input$doubling_time_mean,
  input$doubling_time_sd,
  input$sims
)

recovery_time = sample_gamma(
  input$recovery_time_mean,
  input$recovery_time_sd,
  input$sims
)

hospitalization_rate = sample_beta(
  input$hospitalization_mean, 
  input$hospitalization_weight,
  input$sims
) 

contact_reduction = sample_beta(
  input$contact_reduction_mean, 
  input$contact_reduction_weight,
  input$sims
) 

#--------------------------------------------------
# Fit the model
# -------------------------------------------------

# Initialise the stochastic SIR model
model = StochasticSIR(
  N=input$N, 
  I0=input$I / input$market_share / hospitalization_rate, 
  doubling_time=doubling_time, 
  recovery_time=recovery_time
)

# Generate date from the model
preds = predict(
  model, 
  last_time=input$future_days, 
  contact_reduction=contact_reduction, 
  seed=input$seed
)

#--------------------------------------------------
# Forecast admissions
# -------------------------------------------------
hospital_admissions = forecast_admissions_v(
  preds, 
  input$market_share*hospitalization_rate
)

icu_admissions = forecast_admissions_v(
  preds, 
  input$market_share*hospitalization_rate*input$icu_relative_rate
)

ventilator_admissions = forecast_admissions_v(
  preds, 
  input$market_share*hospitalization_rate*input$ventilator_relative_rate
)

#--------------------------------------------------
# Forecast census
# -------------------------------------------------
hospital_census = forecast_census_v(
  hospital_admissions, 
  length_of_stay = input$hospital_los
)

icu_census = forecast_census_v(
  icu_admissions, 
  length_of_stay = input$hospital_los
)

ventilator_census = forecast_census_v(
  ventilator_admissions, 
  length_of_stay = input$hospital_los
)

#--------------------------------------------------
# Plot admissions
# -------------------------------------------------

admissions_df = bind_rows(
  data.frame(
    ts = seq(1:input$future_days),
    lower  = apply(hospital_admissions, 1, quantile, probs=0.025),
    middle = apply(hospital_admissions, 1, quantile, probs=0.5),
    upper  = apply(hospital_admissions, 1, quantile, probs=0.975),
    typ = 'Hospital'
  ),
  data.frame(
    ts = seq(1:input$future_days),
    lower  = apply(icu_admissions, 1, quantile, probs=0.025),
    middle = apply(icu_admissions, 1, quantile, probs=0.5),
    upper  = apply(icu_admissions, 1, quantile, probs=0.975),
    typ = 'ICU'
  ),
  data.frame(
    ts = seq(1:input$future_days),
    lower  = apply(ventilator_admissions, 1, quantile, probs=0.025),
    middle = apply(ventilator_admissions, 1, quantile, probs=0.5),
    upper  = apply(ventilator_admissions, 1, quantile, probs=0.975),
    typ = 'Ventilator'
  )
)

ggplot(
    data = admissions_df,
    aes(x=ts, group=typ, color=typ)
  ) +
  geom_line(aes(y=middle)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=typ), alpha=0.25)












