source('source/setup.R')
source('source/models.R')
source('source/utils.R')
source('source/forecast.R')

input = list(
  N=4119405,
  I=14,
  market_share=0.15,
  doubling_time=4,
  recovery_time=14,
  length_of_stay=7,
  future_days=60,
  rate_hospitalized=0.025,
  contact_reduction = 0.30
)



# Initialise the basic SIR model
model = BasicSIR(
  N=input$N, 
  I0=input$I / input$market_share / input$rate_hospitalized, 
  doubling_time=input$doubling_time, 
  recovery_time=input$recovery_time
)

# Generate date from the model
preds = predict(
  model, 
  last_time=input$future_days, 
  contact_reduction = input$contact_reduction
)


admits = forecast_admissions(preds, input$rate_hospitalized * input$market_share)
census = forecast_census(preds, input$rate_hospitalized * input$market_share, input$length_of_stay)


