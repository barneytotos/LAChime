#----------------------------
# This is SEIR model with social distancing (no regression)
# The social distancing is done using a sigmoidal curve
#--------------------------

source('source/setup.R')
source('source/plots.R')
source('source/utils.R')

# Global parameters
input = list(
  lag = 13,
  p = 0.05,
  seed = 1337,
  date = mdy('4/11/2020'),
  future = 21
)

# Update the data
set.seed(input$seed)
observed_data = load_ems_data(stop_date = input$date) %>% 
  mutate(date = ymd(date)) 

new_case_data = read.csv('data/franken_data.csv') %>%
  mutate(date = mdy(date)) %>%
  bind_rows(
    observed_data %>% 
      select(date, new_cases) %>% 
      filter(date >= mdy('4/1/2020'))  #' Franken data has a bit of overlap; doesn't match the 'updates' given for 29/30/31 aug
  )

# Roger had me hack this data in for the LA presentation
# new_case_data = new_case_data %>%
#   mutate(new_cases = if_else(date == mdy('4/07/2020'), 298, new_cases))

# Add time
day0 =  min(new_case_data$date) - days(input$lag+1) # this is a pretty important global variable
new_case_data = new_case_data %>% mutate(time = as.numeric(date - day0))

# Load the stan model
SM = stan_model(
  'models/bayes_sigmoidal_seir.stan', 
  auto_write = TRUE
)

# Setup the stan data
stan_data = list(
  # Meta data
  N = 10**7,
  last_time = max(new_case_data$time),
  intervention_time = as.numeric((mdy('3/19/2020') - day0)),
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
  future_intervention = as.numeric(mdy('4/10/2020') - day0),
  future_social = 0.66 # This is a 33% reduction
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
  iter = 10**3 + 10**3, # This is warmup and post warmup
  warmup = 10**3,
  thin = 1,
  chains = 1,
  cores = 1,
  control = list('adapt_delta'=0.99),
  init = inits
)


samples = extract(fit)


print(
  fit, 
  pars=c('E0', 'exposure_time', 'recovery_time', 'doubling_time', 'sigma')
)


print(
  fit, 
  pars=c('social', 'intercept', 'slope')
)


dur = sprintf('plots/fit_%s', max(new_case_data$date))
dir.create(dur, showWarnings = FALSE)
saveRDS(fit, file.path(dur, 'fit.rds')) # this takes forver
saveRDS(
  c(list('new_case_data'= new_case_data, 'observed_data'=observed_data), input),
  file.path(dur, 'params.rds')
)

#------------------------------------
# Plot new cases
#-----------------------------------

#' Adds sampling noise to
forecast_new = function(new_cases, phi, proportion){
  prob = phi / (1 + phi) # convert from stan's 'beta' to r's p
  apply(new_cases, 2, function(x) rnbinom(n=10**3, size=x*proportion*phi, prob=prob)) %>% t
}


new_cases= extract(fit)$projected_newly_infected * input$p
out = apply(new_cases, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% t
colnames(out) = c('Lower', 'Estimate', 'Upper')


plot_data = as.data.frame(out) %>%
  mutate(
    time = 1:n(),
    day = day0 + days(time) + days(input$lag)
  ) %>%
  filter(
    day >= day0,
    day <= input$date + days(input$future) 
  )

ggplot() +
  geom_line(
    data = plot_data ,
    aes(x=day, y=Estimate),
    size=1
  ) +
  geom_ribbon(
    data = plot_data ,
    aes(x=day, ymin = Lower, ymax=Upper),
    fill='orchid',
    alpha = 0.2,
    size = 4
  ) +
  geom_point(
    data = new_case_data,
    aes(x=date, y=new_cases)
  ) +
  geom_vline(
    xintercept = mdy('3/12/2020') + days(input$lag),
    color = 'firebrick',
    linetype = 'dashed',
    size=1
  ) +
  geom_vline(
    xintercept = mdy('3/19/2020') + days(input$lag),
    color = 'firebrick',
    linetype = 'dashed',
    size=1
  ) +
  geom_vline(
    xintercept = input$date,
    color = 'dodgerblue',
    linetype = 'dashed'
  ) +
  theme_bw() +
  scale_y_continuous(
    name = c('Number of new cases'),
    expand = c(0, 0),
    limits = c(0, 1000),
    breaks = seq(0, 1000, 100)
  ) +
  scale_x_date(
    name = '',
    breaks = input$date + days(seq(-28, 28, 7)),
    date_labels = "%m-%d"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1)
  )

ggsave(file.path(dur, 'new_cases.png'), height = 4.5, width=7.5)

#------------------------------------
# Plot social distancing
#------------------------------------
ts = seq(1:(stan_data$last_time + input$lag + 1))
expit = function(x) 1/ (1+exp(-x))
foo = function(x) expit((x - stan_data$intervention_time -samples$intercept)/samples$slope) * (1-samples$social)
socials = map(ts, foo) %>% do.call(cbind, .)

out = apply(socials, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% t
colnames(out) = c('Lower', 'Estimate', 'Upper')


plot_data = as.data.frame(out) %>%
  mutate(
    time = 1:n(),
    day = day0 + days(time) 
  ) %>%
  filter(
    day >= day0,
    day <= input$date + days(1)
  )


ggplot() +
  geom_line(
    data = plot_data ,
    aes(x=day, y=Estimate),
    size=1
  ) +
  geom_ribbon(
    data = plot_data ,
    aes(x=day, ymin = Lower, ymax=Upper),
    fill='orchid',
    alpha = 0.2,
    size = 4
  ) +
  geom_vline(
    xintercept = input$date,
    color = 'dodgerblue',
    linetype = 'dashed',
    size=1
  ) +
  geom_vline(
    xintercept = mdy('3/12/2020'),
    color = 'firebrick',
    linetype = 'dashed',
    size=1
  ) +
  geom_text(
    data = data.frame(
      x = mdy('3/12/2020'),
      y = 0.925,
      label = 'School\nclosures'
    ),
    aes(x=x, y=y, label=label),
    hjust=0,
    nudge_x = -4
  ) +
  geom_text(
    data = data.frame(
      x = mdy('3/19/2020'),
      y = 0.925,
      label = 'Safer-at-home\nordinance'
    ),
    aes(x=x, y=y, label=label),
    hjust=0,
    nudge_x =1
  ) +
  geom_vline(
    xintercept = mdy('3/19/2020'),
    color = 'firebrick',
    linetype = 'dashed',
    size=1
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    name = 'Estimated social distancing (relative reduction)',
    expand = c(0, 0)
  ) +
  scale_x_date(
    name = '',
    limits = c(input$date-days(35), input$date),
    breaks = input$date + days(seq(-35, 0, 7)),
    date_labels = "%m-%d"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1)
  )
ggsave(file.path(dur, 'social_distancing.png'), height = 4.5,  width=7.5)


#------------------------------------
# Plot RT
#------------------------------------
ts = seq(1:(stan_data$last_time + input$lag))
expit = function(x) 1/ (1+exp(-x))
foo = function(x) (1 - expit((x - stan_data$intervention_time -samples$intercept)/samples$slope) * (1-samples$social))*samples$beta/samples$gamma
RT = map(ts, foo) %>% do.call(cbind, .)


out = apply(RT, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% t
colnames(out) = c('Lower', 'Estimate', 'Upper')


plot_data = as.data.frame(out) %>%
  mutate(
    time = 1:n(),
    day = day0 + days(time) 
  ) %>%
  filter(
    day >= day0,
    day <= input$date + 1
  )

ggplot() +
  geom_line(
    data = plot_data ,
    aes(x=day, y=Estimate),
    size=1
  ) +
  geom_ribbon(
    data = plot_data ,
    aes(x=day, ymin = Lower, ymax=Upper),
    fill='orchid',
    alpha = 0.2,
    size = 4
  ) +
  geom_vline(
    xintercept = input$date,
    color = 'dodgerblue',
    linetype = 'dashed',
    size=1
  ) +
  geom_vline(
    xintercept = mdy('3/12/2020'),
    color = 'firebrick',
    linetype = 'dashed',
    size=1
  ) +
  geom_text(
    data = data.frame(
      x = mdy('3/12/2020'),
      y = 0.3,
      label = 'School\nclosures'
    ),
    aes(x=x, y=y, label=label),
    hjust=0,
    nudge_x = -4
  ) +
  geom_text(
    data = data.frame(
      x = mdy('3/19/2020'),
      y = 0.3,
      label = 'Safer-at-home\nordinance'
    ),
    aes(x=x, y=y, label=label),
    hjust=0,
    nudge_x =1
  ) +
  geom_vline(
    xintercept = mdy('3/19/2020'),
    color = 'firebrick',
    linetype = 'dashed',
    size=1
  ) +
  scale_y_continuous(
    limits = c(0, 5),
    breaks = seq(0, 5, 0.5),
    name = latex2exp::TeX('Estimated $R_T$'),
    expand = c(0, 0)
  ) +
  scale_x_date(
    name = '',
    expand = c(0, 0),
    limits = c(input$date-days(35), input$date + days(1)),
    breaks = input$date + days(seq(-35, 0, 7)),
    date_labels = "%m-%d"
  ) +
  theme_bw() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust=1)
  ) 
ggsave(file.path(dur, 'rt.png'), height = 4.5,  width=7.5)


#------------------------------------
# Plot Social distance aparmeters
#------------------------------------
g1 = ggplot(
    data= data.frame(width = 4/samples$slope),
    aes(x=width)
  ) +
  geom_histogram(
    breaks = seq(0, 8, 0.25)
  ) +
  scale_x_continuous(
    name = 'Width of the social distancing window',
    limits = c(0, 8)
  ) +
  theme_bw()



g2 = ggplot(
    data= data.frame(x = stan_data$intervention_time + samples$intercept +3/samples$slope),
    aes(x=x)
  ) +
  geom_histogram(
    breaks = seq(20, 30, 1)
  ) +
  scale_x_continuous(
    breaks = seq(20, 30, 1),
    labels = (day0 + days(seq(20, 30, 1))) %>% format("%m-%d"),
    name = 'First day social distancing > 95%'
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1)
  )

g3 = ggplot(
    data=data.frame(x = 1- samples$social),
    aes(x=x)
  ) +
  geom_histogram(
    breaks =seq(0, 1, 0.05)
  ) +
  scale_x_continuous(
    name = 'Maximum social distancing (relative)',
    expand = c(0, 0)
  ) +
  theme_bw()

g = g1 | g2 | g3
ggsave(file.path(dur, 'social_params.png'), height = 8,  width=13)

#------------------------------------
# Plot model parameters
#------------------------------------
g1 = ggplot(
    data= data.frame(x = samples$doubling_time),
    aes(x=x)
  ) +
  geom_histogram() +
  scale_x_continuous(
    name = 'Doubling time'
  ) +
  theme_bw() 

g2 = ggplot(
    data= data.frame(x = samples$recovery_time),
    aes(x=x)
  ) +
  geom_histogram() +
  scale_x_continuous(
    name = 'Contagious time'
  ) +
  theme_bw() 

g3 = ggplot(
    data= data.frame(x = samples$recovery_time),
    aes(x=x)
  ) +
  geom_histogram() +
  scale_x_continuous(
    name = 'Recovery time'
  ) +
  theme_bw() 

g = g1 | g2 | g3
ggsave(file.path(dur, 'seir_params.png'), height = 8,  width=13)

#------------------------------------
# Create demand estimates
#------------------------------------
new_cases = samples$projected_newly_infected

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


# Cent only
mortality = forecast_new(
  new_cases, 
  samples$phi,
  rep(0.0045, 1000)
)

#--------------------------------------
# Project census
#--------------------------------------
non_icu_census = forecast_census_v(
  non_icu_admissions,
  length_of_stay = 7
)

icu_only_census = forecast_census_v(
  icu_only_admissions,
  length_of_stay = 9
)

vent_census = forecast_census_v(
  vent_admissions,
  length_of_stay = 10
)

icu_census = icu_only_census + vent_census
hospital_census = icu_census + non_icu_census

#--------------------------------------
# Make pretty plots
#--------------------------------------

plot_demand_census(
    hospital_census, 
    los = 7, 
    future_days = 21, 
    day0 = day0, 
    today = input$date,
    lag = 16,
    name='Current total hospitalized, confirmed, COVID patients', 
    color='blue'
  ) + 
  geom_point(
    data = observed_data,
    aes(x=date-day0, y=total_hospital_est)
  ) +
  ggtitle('Census of hospitalized, confirmed, COVID patients')
ggsave(file.path(dur, 'hospital_forecast.png'), height = 5, width = 7)

#--------------------------------------
# Project for ICU patients
#--------------------------------------

plot_demand_census(
    icu_census, 
    los = 9, 
    future_days = input$future, 
    day0 = day0, 
    today = input$date,
    lag = 15,
    name='Current total confirmed COVID patients in the ICU', 
    color='firebrick'
  ) + 
  geom_point(
    data = observed_data,
    aes(x=date-day0, y=total_ICU_confirmed)
  ) +
  ggtitle('Census of confirmed COVID patients in the ICU')
ggsave(file.path(dur, 'icu_forecast.png'), height = 5, width = 7)


#--------------------------------------
# Project for non-ICU patients
#--------------------------------------

plot_demand_census(
    vent_census, 
    los = 7, 
    future_days = input$future, 
    day0 = day0, 
    today = input$date,
    lag = 15,
    name='Current total confirmed COVID patients on ventilator', 
    color='goldenrod'
  ) +
  geom_point(
    data = observed_data,
    aes(x=date-day0, y=total_vent_confirmed)
  ) +
  ggtitle('Census of confirmed COVID patients on ventilator')
ggsave(file.path(dur, 'vent_forecast.png'), height = 5, width = 7)


#--------------------------------------
# Mayor plots
#--------------------------------------

TODAY = mdy('4/10/2020')
FUTURE = 28 + 14 
MY_THEME = theme(
  axis.text.x = element_text(angle = 45, hjust=1, size=10),
  axis.title.y = element_text(size = 12),
  axis.text.y = element_text(size = 10),
  legend.position = c(0.01, 0.99), 
  legend.justification = c(0, 1),
  legend.key.size = unit(1, "line"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=10)
) 


plot_data = data.frame(
    current = apply(samples$projected_newly_infected * input$p, 2, median),
    none = apply(samples$projected_none * input$p, 2, median),
    more = apply(samples$projected_more * input$p, 2, median),
    never = apply(samples$projected_never * input$p, 2, median)
  ) %>%
  mutate(
    time = row_number(),
    day = day0 + days(time) + days(input$lag)
  ) %>%
  tidyr::gather('typ', 'val', -c(time, day))  %>%
  filter(
    day >= TODAY - days(21),
    day <= TODAY + days(FUTURE) 
  ) %>% 
  mutate(typ = factor(typ, levels=c('never', 'none', 'more', 'current'))) %>%
  filter(
    (typ != 'never') | (day <= mdy('5/3/2020'))
  )

ggplot() + 
  geom_line(
    data = plot_data,
    aes(x=day, y=val, group=typ, color=typ),
    size = 2
  ) +
  theme_bw() +
  scale_y_continuous(
    name = c('Newly hospitalized COVID-19 patients'),
    expand = c(0, 0),
    limits = c(0, 20000),
    breaks = seq(0, 20000, 2000)
  ) +
  scale_x_date(
    name = '',
    breaks = TODAY + days(seq(-28, FUTURE, 7)),
    date_labels = "%m-%d"
  ) +
  scale_color_manual(
    values = c(
      'never' = '#F38D68',
      'none' = '#D56062',
      'current' = '#84BCDA',
      'more' = '#70AE6E'
    ),
    labels = c(
      'none' = 'Stop physical distancing',
      'current' = 'Maintain current level',
      'more' = 'More physical distancing',
      'never' = 'Never began distancing'
    ),
    breaks = c(
      'never',
      'none',
      'current',
      'more'
    ),
    name = 'Change in physical distancing'
  ) +
  MY_THEME

ggplot() + 
  geom_line(
    data = plot_data %>% mutate(typ = factor(typ, levels=c('never', 'none', 'more', 'current'))),
    aes(x=day, y=val, group=typ, color=typ),
    size = 2,
    show.legend = FALSE
  ) +
  theme_bw() +
  scale_y_continuous(
    name = c('Newly hospitalized COVID-19 patients'),
    expand = c(0, 0),
    limits = c(0, 871),
    breaks = seq(0, 800, 100)
  ) +
  scale_x_date(
    name = '',
    limits = mdy('4/10/2020') + days(c(-21, 21)),
    breaks = mdy('4/10/2020') + days(seq(-28, FUTURE, 7)),
    date_labels = "%m-%d"
  ) +
  scale_color_manual(
    values = c(
      'never' = '#F38D68',
      'none' = '#D56062',
      'current' = '#84BCDA',
      'more' = '#70AE6E'
    ),
    labels = c(
      'none' = 'Stop physical distancing',
      'current' = 'Maintain current level',
      'more' = 'More physical distancing',
      'never' = 'Never began distancing'
    ),
    name = 'Change in physical distancing'
  ) +
  MY_THEME +
  theme(axis.title.y = element_blank())



#--------------------------------------
# Mortality
#--------------------------------------

mortality = forecast_new(
  extract(fit)$projected_newly_infected, 
  samples$phi,
  rep(0.0045, 1000)
)



out = apply(mortality, 2, quantile, probs = c(0.025, 0.5, 0.975)) %>% t
colnames(out) = c('Lower', 'Estimate', 'Upper')

plot_data = as.data.frame(out) %>%
  mutate(
    time = 1:n(),
    day = day0 + days(time) + days(input$lag + 12)
  ) %>%
  filter(
    day >= day0,
    day <= input$date + days(input$future) 
  )

ggplot() +
  geom_line(
    data = plot_data ,
    aes(x=day, y=Estimate),
    size=1
  ) +
  geom_ribbon(
    data = plot_data ,
    aes(x=day, ymin = Lower, ymax=Upper),
    fill='grey',
    alpha = 0.2,
    size = 4
  ) +
  geom_vline(
    xintercept = input$date,
    color = 'dodgerblue',
    linetype = 'dashed'
  ) +
  theme_bw() +
  scale_y_continuous(
    name = c('Number of deaths'),
    expand = c(0, 0),
    limits = c(0, 200),
    breaks = seq(0, 200, 20)
  ) +
  scale_x_date(
    name = '',
    breaks = input$date + days(seq(-28, 28, 7)),
    date_labels = "%m-%d"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1)
  )

ggsave(file.path(dur, 'mortality.png'), height = 4.5, width=7.5)
