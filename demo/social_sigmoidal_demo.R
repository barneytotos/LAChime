#----------------------------
# This is SEIR model with social distancing (no regression)
# The social distancing is done using a sigmoidal model
#--------------------------

source('source/setup.R')
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
# dur = file.path('presentations', '07APR20')
dir.create(dur, showWarnings = FALSE)

#------------------------------------
# Plot new cases
#-----------------------------------
FUTURE = 21 

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
    day <= today() + days(FUTURE) 
  )

pred_cases = samples$sampled_newly_infected 
out = apply(pred_cases, 2, quantile, probs = c(0.025, 0.975)) %>% t
plot_data$low_low = out[1:nrow(plot_data), 1]
plot_data$hi_hi = out[1:nrow(plot_data), 2]

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
    xintercept = today(),
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
    breaks = today() + days(seq(-28, 28, 7)+1),
    date_labels = "%m-%d"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust=1)
  )

ggsave(file.path(dur, 'new_cases.png'), height = 4.5, width=7.5)

#------------------------------------
# Plot social distancing
#------------------------------------
FUTURE = input$lag
ts = seq(1:(stan_data$last_time + FUTURE))
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
    day <= today() + days(FUTURE-input$lag)
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
    xintercept = today(),
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
    limits = c(today()-days(35), today()),
    breaks = today() + days(seq(-35, 0, 7)),
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
FUTURE = input$lag+1
ts = seq(1:(stan_data$last_time + FUTURE))
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
    day <= today() + days(FUTURE-input$lag)
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
    xintercept = today(),
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
    limits = c(today()-days(35), today() + days(1)),
    breaks = today() + days(seq(-35, 0, 7)),
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
