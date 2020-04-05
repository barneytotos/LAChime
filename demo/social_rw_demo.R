#----------------------------
# This is SEIR model with social distancing (no regression)
# The social distancing is done using a random walk that is strictly decreasing
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
    date = ymd(date),
    lag_date = ymd(date) - days(input$lag)
  )

# Add time
day0 =  min(new_case_data$date) - days(input$lag+1)
new_case_data = new_case_data %>% mutate(time = as.numeric(date - day0))


# Load the stan model
SM = stan_model(
  'models/bayes_social_rw_seir.stan', 
  auto_write = TRUE
)

# Setup the stan data
stan_data = list(
  # Meta data
  N = 10**7,
  last_time = max(new_case_data$time),
  intervention_time = as.numeric((mdy('3/12/2020') - day0)),
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
  tau = 0.175
)

# Model initilization
inits = function(chain_id) {
  list(
    'recovery_time'=10.0, 
    'doubling_time'=3, 
    'exposure_time'=2.5,
    'sigma'=1.5,
    'E0'=10,
    'sigma'=3
  )
}

fit = sampling(
  SM,
  stan_data,
  iter = 10**3 + 10**3,
  warmup = 10**3,
  thin = 1,
  chains = 1,
  cores =14,
  control = list('adapt_delta'=0.99),
  init = inits
)


print(
  fit, 
  pars=c('E0', 'exposure_time', 'recovery_time', 'doubling_time', 'sigma')
)

print(
  fit, 
  pars=c('eta', 'social')
)


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
    day <= today() + days(8) + days(21)
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
    xintercept = today(),
    color = 'dodgerblue',
    linetype = 'dashed'
  ) +
  theme_bw() +
  scale_y_continuous(
    #limits = c(0, 500),
    name = c('# of confirmed cases'),
    # breaks = seq(0, 5000, 500),
    #expand = c(0, 0)
  ) 
ggsave('random_walk.png', height = 5, width = 6)






