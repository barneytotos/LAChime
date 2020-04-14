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


# Update the data
observed_data = load_ems_data() %>% mutate(date = mdy(date))

new_case_data = read.csv('data/franken_data.csv') %>%
  mutate(date = mdy(date)) %>%
  bind_rows(
    observed_data %>% 
      select(date, new_cases) %>% 
      filter(date > mdy('3/27/2020'))
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
  step_size = 12,
  # Future stuff,
  future_intervention = as.numeric(today() - day0),
  future_social = 0.66
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
    'data/ems_clean_2020-04-06.csv'
  ) %>%
  mutate(
    date = ymd(date),
    lag_date = ymd(lag_date)
  )


# Create the data frame
# Big old pivot
observed_data = list.files('data/la_hospital/', full.names = TRUE) %>%
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
    lag_date = date - days(input$lag),
    total_non_icu = Confirm_med.surg + Confirm_SDU
  ) %>%
  select(
    date,
    lag_date,
    time,
    total_hospital_est = total_estimated,
    total_icu_est = total_ICU_est,
    total_vent_est = total_vent_est,
    total_hospital_confirmed = total_confirmed,
    total_ICU_confirmed = Confirm_ICU,
    total_vent_confirmed = Confirm_vent
  )


forecast_new = function(new_cases, phi, proportion){
  prob = phi / (1 + phi) # convert from stan's 'beta' to r's p
  apply(new_cases, 2, function(x) rnbinom(n=10**3, size=x*proportion*phi, prob=prob)) %>% t
}
FUTURE = 22
dur = sprintf('plots/fit_%s', max(new_case_data$date))
# dur = file.path('presentations', '07APR20')
dir.create(dur, showWarnings = FALSE)
#--------------------------------------
# Project admissions
#--------------------------------------

# Non-icu
non_icu_admissions = forecast_new(
  new_cases, 
  samples$phi,
  rep(0.025, 1000)
)


# ICU only
icu_only_admissions = forecast_new(
  new_cases, 
  samples$phi,
  rep(0.01*0.1, 1000)
)

# Cent only
vent_admissions = forecast_new(
  new_cases, 
  samples$phi,
  rep(0.01*0.9, 1000)
)

#--------------------------------------
# Project census
#--------------------------------------

non_icu_census = forecast_census_v(
  non_icu_admissions,
  7
)

icu_only_census = forecast_census_v(
  icu_only_admissions,
  9
)

vent_census = forecast_census_v(
  vent_admissions,
  10
)

icu_census = icu_only_census + vent_census
hospital_census = icu_census + non_icu_census



#--------------------------------------
# Make pretty plots
#--------------------------------------

plot_demand3(
    hospital_census, 
    los = 7, 
    future_days = 21, 
    day0 = day0, 
    lag = 16,
    name='Current total hospitalized, confirmed, COVID patients', 
    color='blue'
  ) + 
  # geom_point(
  #  data = observed_data,
  #  aes(x=date-day0, y=total_hospital_est)
  #) +
  ggtitle('Census of hospitalized, confirmed, COVID patients')
ggsave(file.path(dur, 'hospital_forecast.png'), height = 5, width = 7)

# non_icu_tab = census_table(
#  admissions=non_icu_admissions, 
#  los=non_icu_los, 
#  day0=day0, 
#  lag = 15,
#  thresholds =  c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)
# )
# write.csv(non_icu_tab ,'presentations/07APR20/non_icu_threshold.csv')
hospital_census[mdy('4/10/2020') - day0 - 16, ] %>% quantile(c(0.025, 0.5, 0.975))
#--------------------------------------
# Project for ICU patients
#--------------------------------------

plot_demand3(
    icu_census, 
    los = 9, 
    future_days = FUTURE, 
    day0 = day0, 
    lag = 15,
    name='Current total confirmed COVID patients in the ICU', 
    color='firebrick'
  ) + 
  #geom_point(
  #  data = observed_data,
  #  aes(x=date-day0, y=total_ICU_confirmed)
  #) +
  ggtitle('Census of confirmed COVID patients in the ICU')
ggsave(file.path(dur, 'icu_forecast.png'), height = 5, width = 7)

icu_census[mdy('4/10/2020') - day0 - 16, ] %>% quantile(c(0.025, 0.5, 0.975))

#icu_tab = census_table(
#  admissions=icu_admissions, 
#  los=icu_los, 
#  day0=day0, 
#  lag = 15,
#  thresholds = c(125, 250, 500, 1000, 2000, 4000, 8000, 16000)
#)
#write.csv(icu_tab ,'presentations/07APR20/icu_threshold.csv')
#--------------------------------------
# Project for non-ICU patients
#--------------------------------------
plot_demand3(
    vent_census, 
    los = 7, 
    future_days = FUTURE, 
    day0 = day0, 
    lag = 15,
    name='Current total confirmed COVID patients on ventilator', 
    color='goldenrod'
  ) +
  # geom_point(
  #  data = observed_data,
  #  aes(x=date-day0, y=total_vent_confirmed)
  #) +
  ggtitle('Census of confirmed COVID patients on ventilator')
ggsave(file.path(dur, 'vent_forecast.png'), height = 5, width = 7)

vent_census[mdy('4/10/2020') - day0 - 16, ] %>% quantile(c(0.025, 0.5, 0.975))

# vent_tab = census_table(
#  admissions=vent_admissions, 
#  los=vent_los, 
#  day0=day0, 
#  lag = 15,
#  thresholds =  c(100, 200, 400, 800, 1600, 3200, 6400, 12800)
#)
#write.csv(vent_tab ,'presentations/07APR20/vent_threshold.csv')


#estimate = apply(new_cases*input$p, 2, quantile, probs=0.5)
#observed_data %>%
#  mutate(
##    estimated_new_cases = estimate[observed_data$time+16]
#  ) %>%
#  write.csv('kerts_data.csv')

