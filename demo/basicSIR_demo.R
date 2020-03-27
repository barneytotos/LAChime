#--------------------------------------------------
# This file contains a demo of the computations performed at https://github.com/CodeForPhilly/chime/tree/develop/src/penn_chime
# Particularly, it shows similar functionality as penn_chime.models.SimSirModel
# -------------------------------------------------
source('source/setup.R')
source('source/models.R')
source('source/utils.R')
source('source/forecast.R')

# This mimics the parameters object in 
input = list(
  N=4119405,
  I=14,
  market_share=0.15,
  doubling_time=4,
  recovery_time=14,
  future_days=50,
  rate_hospitalized=0.025,
  contact_reduction=0.30,
  # Hospital forecasting
  hospital_length_of_stay=7,
  hospital_rate=0.025,          # should be the same as rate_hospitalize
  # ICU forecasting
  icu_length_of_stay=9,
  icu_rate=0.025*0.3 ,
  # Ventilator forecasting
  ventilator_length_of_stay=10,
  ventilator_rate=0.025 *.2
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
plot(model, preds) 


#-------------------------------------------
# Forecasting admissions
#-------------------------------------------

hospital_admits = forecast_admissions(
  preds, 
  input$hospital_rate * input$market_share
)

icu_admits = forecast_admissions(
  preds, 
  input$icu_rate * input$market_share
)

ventilator_admits = forecast_admissions(
  preds, 
  input$ventilator_rate * input$market_share
)

# Make a quick plot
admit_df = bind_rows(
  data.frame(ts=1:length(hospital_admits), ys=hospital_admits, typ='Hospitalized'),
  data.frame(ts=1:length(icu_admits), ys=icu_admits, typ='ICU'),
  data.frame(ts=1:length(ventilator_admits), ys=ventilator_admits, typ='Hospital')
)

ggplot(
    admit_df,
    aes(x=ts, y=ys, group=typ, color=typ)
  ) +
  geom_line() +
  geom_point() +
  scale_x_continuous(name='Days from today')+
  scale_y_continuous(name='Admissions') +
  theme_bw()

#-------------------------------------------
# Forecasting census
#-------------------------------------------

hospital_census = forecast_census(
  preds, 
  input$hospital_rate * input$market_share, 
  input$hospital_length_of_stay
)

icu_census = forecast_census(
  preds, 
  input$icu_rate * input$market_share, 
  input$icu_length_of_stay
)

ventilator_census = forecast_census(
  preds, 
  input$ventilator_rate * input$market_share, 
  input$ventilator_length_of_stay
)

# Make a quick plot
census_df = bind_rows(
  data.frame(ts=1:length(hospital_census), ys=hospital_census, typ='Hospitalized'),
  data.frame(ts=1:length(icu_census), ys=icu_census, typ='ICU'),
  data.frame(ts=1:length(ventilator_census), ys=ventilator_census, typ='Hospital')
)

ggplot(
    census_df,
    aes(x=ts, y=ys, group=typ, color=typ)
  ) +
  geom_line() +
  geom_point() +
  scale_x_continuous(name='Days from today')+
  scale_y_continuous(name='Admissions') +
  theme_bw()


