source('../source/setup.R')
source('../source/models.R')
source('../source/utils.R')
source('../source/forecast.R')
source('../source/plots.R')

library('patchwork')
library('rstan')
library(gridExtra)
library(grid)
library(gtable)
library('lubridate')

# Load the data
lax_data = readr::read_csv('../data/lax_county_data_26MAR20.csv') %>%
  mutate(date = as.Date(day)) %>%
  select(-day)



# Read the files
files = list.files('../data/la_hospital/', full.names = TRUE)
print(files)
df = data.frame()
for (f in files) df = bind_rows(df, readr::read_csv(f) %>% mutate(Date = mdy(Date)))

#--------------------------------------------------
# Some date stuff
# -------------------------------------------------
day0 = mdy('2/29/2020')
t0 = as.numeric(today() - day0)

#--------------------------------------------------
# transform the data
# -------------------------------------------------


# Add a time delta component
df = df %>% mutate(
  t_real = as.numeric(Date - day0)
)

# Estimate the number of new cases each day
model_df = df %>% 
  filter(Category %in% c('24PUI_med.surg', '24PUI_SDU', '24PUI_ICU')) %>%
  mutate(
    weight = 0.25
  ) %>%
  group_by(
    Date
  ) %>%
  mutate(
    avg_market_share = mean(Marketshare),
    estimated_cases = Value * Marketshare / avg_market_share * weight
  ) %>%
  group_by(
    Date,
    avg_market_share,
    t_real
  ) %>%
  summarise(
    estimated_cases = sum(estimated_cases)
  )



expose_stan_functions(stan_model('../models/bayes_seir.stan', auto_write = TRUE))


server <- function(input, output) {

  #-------------------------------------------
  # Run the model and update the predictions
  #-------------------------------------------
  
  MODEL = eventReactive(input$runButton, {
    
    # This bit will be come reactive to the data
    set.seed(1337)
    
    df = lax_data
    t0 = max(nrow(df), 14)
    
    # Init the model
    model = BayesSEIR(
      10**7,
      exposure_mean=input$exposure_mean,
      exposure_sd=input$exposure_sd,
      recovery_mean=input$recovery_mean,
      recovery_sd=input$recovery_sd,
      doubling_mean=input$doubling_mean,
      doubling_sd=input$doubling_sd,
      stan_fname='../models/bayes_seir.stan'
    )
    
    # Fit 
    model = fit(
      model, 
      df$time, 
      df$new_cases,
      p = input$p
    )
    
    print(model$fit, pars =c('exposure_time', 'recovery_time', 'doubling_time', 'phi'))

    model
  })
  
  # "updates the things
  PREDS = eventReactive(input$runButton, {
    
    model = MODEL()
    
    # Some timing and social distancing things
    lag = as.numeric(input$lag)
    social = 1-as.numeric(input$social)
    full_social = 19
    start_social = 12
    
    contact_reduction = function(x) case_when(
      x <= start_social + lag ~ 1,
      x <= full_social + lag ~ 1 - (1-social) * (x-(start_social+lag)) / 7,
      TRUE ~ social
    )
    
    #  Do the predictions
    preds = predict(
      model, 
      last_time=t0+180+input$lag, 
      contact_reduction=contact_reduction
    )
    
    phi = extract(model$fit)$phi
    
    # Predict admissions
    new_cases = forecast_admissions_nb(
      preds, 
      rep(input$p, 1000),
      phi
    )
    
    # Predict admissions
    hospital_admissions = forecast_admissions_nb(
      preds, 
      rep(input$hospital_relative_rate, 1000),
      phi
    )
    
    # Predict admissions
    icu_admissions = forecast_admissions_nb(
      preds, 
      rep(input$icu_relative_rate, 1000),
      phi
    )
    
    # Predict admissions
    ventilator_admissions = forecast_admissions_nb(
      preds, 
      rep(input$ventilator_relative_rate, 1000),
      phi
    )
    
    # Do the predictions
    list(
      # Predicions for the plots
      new_cases = new_cases,
      hospital_admissions = hospital_admissions,
      icu_admissions = icu_admissions,
      ventilator_admissions = ventilator_admissions,
      t0 = as.numeric(lubridate::today()-lubridate::mdy('2/29/2020')),
      samples = extract(model$fit, pars = c('recovery_time', 'doubling_time', 'exposure_time'))
    )
    
    
  })
  
  #-------------------------------------------
  # Plots by demand type
  #-------------------------------------------
  
  output$icu <- renderImage({
    
    # Plot new admissions
    g1 = plot_admissions(
      PREDS()$icu_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ICU (non-ventilator) patients', 
      color='firebrick'
    )
    
    # Plot total census
    g2 = plot_demand(
      PREDS()$icu_admissions, 
      los = input$icu_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ICU (non-ventilator) patients', 
      color='firebrick'
    )
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g1 | g2, height = 6, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 6*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)

  output$hospital <- renderImage({
    
    # Plot new admissions
    g1 = plot_admissions(
      PREDS()$hospital_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='hospital patients', 
      color='blue'
    )
    
    # Plot total census
    g2 = plot_demand(
      PREDS()$hospital_admissions, 
      los = input$hospital_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='hospital patients', 
      color='blue'
    )
    
    
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g1 | g2, height = 6, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 6*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$ventilator <- renderImage({
    
    # Plot new admissions
    g1 = plot_admissions(
      PREDS()$ventilator_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ventilator patients', 
      color='goldenrod'
    )
    
    # Plot total census
    g2 = plot_demand(
      PREDS()$ventilator_admissions, 
      los = input$ventilator_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ventilator patients', 
      color='goldenrod'
    )
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g1 | g2, height = 6, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 6*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$new_cases <- renderImage({
    
    #--------------------------------------------------
    # Get the predictions
    # -------------------------------------------------
    
    # Get the new data
    observed_df = lax_data %>% mutate(ts = time-PREDS()$t0)
    
    
    full_social = as.numeric(lubridate::mdy('3/19/2020') - lubridate::today())
    start_social = as.numeric(lubridate::mdy('3/12/2020') - lubridate::today())
    
    
    # Plot new admissions
    g = plot_admissions(
        PREDS()$new_cases, 
        future_days = input$future_days, 
        day0 = nrow(lax_data), 
        lag = input$lag,
        name='confirmed infected', 
        color='purple'
      ) +
      geom_line(
        data = observed_df,
        aes(x=ts-input$lag, y=new_cases),
        size=2,
        color='orchid'
      ) +
      geom_vline(
        xintercept = full_social,
        color = 'black',
        linetype = 'dashed'
      ) + 
      geom_vline(
        xintercept = start_social,
        color = 'black',
        linetype = 'dashed'
      ) + 
      ylab('New confirmed cases')

    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 6, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 6*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$data <- renderImage({
    
    #--------------------------------------------------
    # Get the predictions
    # -------------------------------------------------
    day0 = lubridate::mdy('2/29/2020')
    t0 = as.numeric(lubridate::today() - day0)
    
    full_social = lubridate::mdy('3/19/2020') 
    start_social = lubridate::mdy('3/12/2020') 
    
    g <- ggplot() +
      geom_line(
        data = lax_data,
        aes(x=time, y=new_cases, color='LAX')
      ) +
      geom_point(
        data = lax_data,
        aes(x=time, y=new_cases, color='LAX')
      ) +
      geom_line(
        data = lax_data,
        aes(x=time-input$lag, y=new_cases, color='LAX'),
        linetype='dashed'
      ) +
      geom_point(
        data = lax_data,
        aes(x=time-input$lag, y=new_cases, color='LAX')
      ) +
      geom_line(
        data = model_df,
        aes(x=t_real, y=estimated_cases/avg_market_share/0.25, color='HOSP')
      ) +
      geom_point(
        data = model_df,
        aes(x=t_real, y=estimated_cases/avg_market_share/0.25, color='HOSP')
      ) +
      #geom_point(
      #  data = model_df,
      #  aes(x=t_real, y=estimated_cases/avg_market_share/0.25, color='HOSP')
      #) +
      geom_vline(
        xintercept = today() - day0,
        aes(color='today'),
        linetype='dashed',
        size = 1
      ) + 
      geom_vline(
        xintercept = mdy('3/12/2020') - day0,
        aes(color='schools'),
        linetype='dashed',
        size = 1
      ) +  
      geom_vline(
        xintercept = mdy('3/19/2020') - day0,
        aes(color='schools'),
        linetype='dashed',
        size = 1
      ) +  
      geom_text(
        data = data.frame(x=mdy('3/12/2020') - day0-1, y=450, lab='School\nClosure'),
        aes(x=x, y=y, label=lab),
        hjust='right'
      ) +
      geom_text(
        data = data.frame(x=mdy('3/19/2020') - day0-1, y=450, lab='Full\nDistancing'),
        aes(x=x, y=y, label=lab),
        hjust='right'
      ) +
      geom_text(
        data = data.frame(x=today() - day0-1, y=450, lab='Today\n'),
        aes(x=x, y=y, label=lab),
        hjust='right'
      ) +
      scale_x_continuous(
        name = 'Date',
        breaks = seq(-14, 31, 7),
        labels = (day0 + days(seq(-14, 31, 7))) %>% format("%m-%d"),
        limits = c(-14, 31),
        expand = c(0,0 )
      ) +
      scale_y_continuous(
        name = 'Count',
        limits = c(-10, 500),
        expand = c(0, 0),
        breaks = seq(0, 450, 50)
      ) +
      scale_color_manual(
        values = c(
          'LAX'='Orchid', 
          'HOSP'=ORANGE
        ),
        labels = c(
          'LAX'='DPH newly confirmed cases',
          'HOSP'='EMS total new PUI\n(weighted by marketshare)\n'
        )
      ) +
      theme_bw() +
      theme(
        legend.position = c(0.01, 0.99), 
        legend.justification = c(0, 1),
        legend.key.size = unit(1, "line"),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA)
      )
    
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 6, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 6*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  #--------------------------------------
  # Plots by type
  #--------------------------------------
  output$new_demand <- renderImage({

    # Plot new admissions
    g1 = plot_admissions(
      PREDS()$hospital_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='hospital admissions', 
      color='blue'
    ) + ggtitle('New hospital admissions')
    
    
    # Plot new admissions
    g2 = plot_admissions(
      PREDS()$icu_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ICU (non-ventilator) patients', 
      color='firebrick'
    ) + ggtitle('New ICU (non-ventilator) patients')
    
    # Plot new admissions
    g3 = plot_admissions(
      PREDS()$ventilator_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ventilator', 
      color='goldenrod'
    ) + ggtitle('New ventilator patients')
    
    g = g1 | g2 | g3
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 5, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 5*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$total_demand <- renderImage({
    
    # Plot new admissions
    g1 = plot_demand(
      PREDS()$hospital_admissions, 
      los = input$hospital_los, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='hospital patients', 
      color='blue'
    ) + ggtitle('Current total hospital patients')
    
    
    # Plot new admissions
    g2 = plot_demand(
      PREDS()$icu_admissions, 
      los = input$icu_los, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ICU (non-ventilator) patients', 
      color='firebrick'
    ) + ggtitle('Current total ICU (non-ventilator) patients')
    
    # Plot new admissions
    g3 = plot_demand(
      PREDS()$ventilator_admissions, 
      los = input$ventilator_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = input$lag,
      name='ventilator patients', 
      color='goldenrod'
    ) + ggtitle('Current total ventilator patients')
    
    g = g1 | g2 | g3
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 5, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 5*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  #---------------------------------------
  # Posterior plots
  #---------------------------------------
  
  output$plot_doubling_time <- renderImage({
    
    samples = data.frame(
      doubling_time = PREDS()$samples$doubling_time
    ) 
    
    g <- ggplot(
        data = samples,
        aes(x=doubling_time)
      ) +
      geom_histogram(
        aes(y = stat(count) / sum(count)),
        breaks = seq(1, 5, 0.1)
      ) +
      scale_x_continuous(
        name = 'Doubling time'
      ) +
      scale_y_continuous(
        limits = c(0, 0.5),
        name = 'Proportion'
      ) +
      theme_bw() #+
      # ggtitle(sprintf('Doubling time: estimate = %.1f +/- %.2f', mean(samples$doubling_time), sd(samples$doubling_time)))
    
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 4, width=4)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 4*105,
      height = 4*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$plot_recovery_time <- renderImage({
    
    samples = data.frame(
      recovery_time = PREDS()$samples$recovery_time
    ) 
    
    g <- ggplot(
        data = samples,
        aes(x=recovery_time)
      ) +
      geom_histogram(
        aes(y = stat(count) / sum(count))
      ) +
      scale_x_continuous(
        name = "Average days contagious"
      ) +
      scale_y_continuous(
        limits = c(0, 0.5),
        name = 'Proportion'
      ) +
      theme_bw() 
      #ggtitle(sprintf('Contagious time: estimate = %.0f +/- %.0f', mean(samples$recovery_time), sd(samples$recovery_time)))
    
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 4, width=4)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 4*105,
      height = 4*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$plot_exposure_time <- renderImage({
    
    samples = data.frame(
      exposure_time = PREDS()$samples$exposure_time
    ) 
    
    g <- ggplot(
        data = samples,
        aes(x=exposure_time)
      ) +
      geom_histogram(
        aes(y = stat(count) / sum(count))
      ) +
      scale_x_continuous(
        name = 'Average time from exposure to infectious'
      ) +
      scale_y_continuous(
        limits = c(0, 0.5),
        name = 'Proportion'
      ) +
      theme_bw() 

    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 4, width=4)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 4*105,
      height = 4*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  #---------------------------------------
  # Threshold tables
  #---------------------------------------
  
  output$hospital_thresholds = renderTable({
    
    # Make the tables
    census_table(
      admissions=PREDS()$hospital_admissions, 
      los=input$hospital_los, 
      t0=PREDS()$t0, 
      thresholds = c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000),
      lag=input$lag
    )
    
    
    
  })
  
  output$icu_thresholds = renderTable({
    
    # Make the tables
    census_table(
      admissions=PREDS()$icu_admissions, 
      los=input$icu_los, 
      t0=PREDS()$t0, 
      thresholds = c(125, 250, 500, 1000, 2000, 4000, 8000, 16000),
      lag=input$lag
    )
    
    
    
  })
  
  output$ventilator_thresholds = renderTable({
    
    # Make the tables
    census_table(
      admissions=PREDS()$ventilator_admissions, 
      los=input$ventilator_los, 
      t0=PREDS()$t0, 
      thresholds = c(100, 200, 400, 800, 1600, 3200, 6400, 12800),
      lag=input$lag
    )
    
    
    
  })
  
  #---------------------------------------
  # Posterior estimates
  #---------------------------------------
  
  output$est_doubling_mean <- renderText({
    if (is.null(PREDS()$samples$doubling_time)){
      return(NULL)
    }
    
    PREDS()$samples$doubling_time %>% mean() %>% round(1) %>% as.character()
  })
  
  output$est_recovery_mean <- renderText({
    if (is.null(PREDS()$samples$recovery_time)){
      return(NULL)
    }
    
    PREDS()$samples$recovery_time %>% mean() %>% round(1) %>% as.character()
  })
  
  output$est_exposure_mean <- renderText({
    if (is.null(PREDS()$samples$exposure_time)){
      return(NULL)
    }
    
    PREDS()$samples$exposure_time %>% mean() %>% round() %>% as.character()
  })
  
  output$est_doubling_sd <- renderText({
    if (is.null(PREDS()$samples$doubling_time)){
      return(NULL)
    }
    
    PREDS()$samples$doubling_time %>% sd() %>% round(2) %>% as.character()
  })
  
  output$est_recovery_sd <- renderText({
    if (is.null(PREDS()$samples$recovery_time)){
      return(NULL)
    }
    
    PREDS()$samples$recovery_time %>% sd() %>% round(1) %>% as.character()
  })
  
  output$est_exposure_sd <- renderText({
    if (is.null(PREDS()$samples$exposure_time)){
      return(NULL)
    }
    
    PREDS()$samples$exposure_time %>% sd() %>% round(2) %>% as.character()
  })
  
  #---------------------------------------
  # Table of the data
  #---------------------------------------
  
  output$contents = renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame 
    return(lax_data %>% select(time, new_confirmed_cases=new_cases))
    
  })
  
}



