source('../source/setup.R')
source('../source/models.R')
source('../source/utils.R')
source('../source/forecast.R')
source('../source/plots.R')

library('patchwork')
library('rstan')
library('lubridate')


# --------------------------------
# Global variables
# -------------------------------
START_SOCIAL = mdy('3/12/2020')
FULL_SOCIAL = mdy('3/19/2020')
DEMAND_LAG = 13

dir.create('plots', showWarnings = FALSE)

server <- function(input, output) {

  #-------------------------------------------
  # Run the model and update the predictions
  #------------------------------------------
  DF = eventReactive(input$runButton, {
    
    # Load the data
    if (input$dataSet=='dph'){
      
      # DPH data with not problems
      df <- readr::read_csv('../data/lax_county_data_26MAR20.csv') %>% 
        mutate(
          date = mdy(day),
          time = as.numeric(date - min(date)) + 1,
          lag_date = date - days(input$lag),
          lag_time = as.numeric(lag_date - min(lag_date)) + 1
        )
        
    } else if (input$dataSet=='dph_last_10'){
      
      # DOF data with only the last 10
      df <- readr::read_csv('../data/lax_county_data_26MAR20.csv') %>% 
        tail(10) %>%
        mutate(
          date = mdy(day),
          time = as.numeric(date - min(date)) + 1,
          lag_date = date - days(input$lag),
          lag_time = as.numeric(lag_date - min(lag_date)) + 1
        )
      
    } else if (input$dataSet=='ems'){
      
      df <- readr::read_csv('../data/la_ems_30MAR20.csv') %>%
        mutate(
          date = mdy(day),
          time = as.numeric(date - min(date)) + 1,
          lag_date = date - days(input$lag),
          lag_time = as.numeric(lag_date - min(lag_date)) + 1
        )
      
    } else if (input$dataSet=='pui'){
      
      df <- readr::read_csv('../data/joe_ems_31MAR20.csv') %>%
        mutate(
          date = mdy(day),
          time = as.numeric(date - min(date)) + 1,
          lag_date = date - days(input$lag),
          lag_time = as.numeric(lag_date - min(lag_date)) + 1
        )
    } else {
      stop('DATA SET NOT SPECIFIED')
    }
    
    # Setup global time variables
    day0 = min(df$lag_date) - days(1)
    list(
      df = df,
      day0 = day0,
      t0 = as.numeric(today() - day0)
    )
    
  })
  
  MODEL = eventReactive(input$runButton, {
    
    # This bit will be come reactive to the data
    set.seed(1337)
    
    df = DF()$df
    
    # Init the model
    model = BayesSEIR(
      10**7,
      exposure_mean=2.5,
      exposure_sd=1.5,
      recovery_mean=input$recovery_mean,
      recovery_sd=input$recovery_sd,
      doubling_mean=input$doubling_mean,
      doubling_sd=input$doubling_sd,
      stan_fname='../models/bayes_seir.stan'
    )
    
    # Fit 
    model = fit(
      model, 
      df %>% pull(lag_time), 
      df %>% pull(new_cases),
      p = input$p
    )
    
    print(model$fit, pars =c('exposure_time', 'recovery_time', 'doubling_time', 'phi'))

    model
  })
  
  # "updates the things
  PREDS = eventReactive(input$runButton, {
    
    # Get the new data
    # @todo: unfuck this section
    df = DF()$df
    day0 = min(df$lag_date) - days(1)
    t0 = as.numeric(today() - day0)
    model = MODEL()
    
    # Social distancing
    start = as.numeric(START_SOCIAL - day0)
    stop = as.numeric(FULL_SOCIAL - day0)
    social = 1-as.numeric(input$social)
    
    contact_reduction = function(x) case_when(
      x <= start ~ 1,
      x <= stop  ~ 1 - (1-social) * (x-start) / 7,
      TRUE ~ social
    )
    
    # Do the predictions
    preds = predict(
      model, 
      last_time = t0 + 180,
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
      rep(input$icu_relative_rate, 1000)
    )
    
    # Predict admissions
    ventilator_admissions = forecast_new(
      model,
      preds, 
      rep(input$ventilator_relative_rate, 1000)
    )
    
    # Do the predictions
    list(
      # Predicions for the plots
      preds = preds,
      new_cases = new_cases,
      hospital_admissions = hospital_admissions,
      icu_admissions = icu_admissions,
      ventilator_admissions = ventilator_admissions,
      t0 = as.numeric(lubridate::today()-lubridate::mdy('2/29/2020')),
      samples = extract(model$fit, pars = c('recovery_time', 'doubling_time', 'exposure_time', 'beta', 'gamma'))
    )

  })
  
  #-------------------------------------------
  # Plots by demand type
  #-------------------------------------------
  
  output$new_cases <- renderImage({
    
    #--------------------------------------------------
    # Get the predictions
    # -------------------------------------------------
    
    # Get the new data
    df = DF()$df
    day0 = DF()$day0
    t0 = DF()$t0

    # 
    full_social = as.numeric(lubridate::mdy('3/19/2020') - lubridate::today())
    start_social = as.numeric(lubridate::mdy('3/12/2020') - lubridate::today())
    
    # Some labeling stuff
    xmax = as.numeric((today() + days(input$future_days)) - day0)
    x_breaks = seq(0, xmax, 7)
    
    # Plot new admissions
    g1 = plot_admissions(
        # PREDS()$new_cases, 
        PREDS()$preds[, 5, ], 
        future_days = input$future_days, 
        day0 = day0, 
        lag = 0,
        name='exposed individuals', 
        color='purple'
      ) +
      geom_line(
        data = df,
        aes(x=as.numeric(lag_date-day0), y=new_cases/input$p),
        size=2,
        color='orchid'
      ) +
      geom_vline(
        xintercept = FULL_SOCIAL - today(),
        color = 'black',
        linetype = 'dashed'
      ) + 
      geom_vline(
        xintercept = START_SOCIAL - today(),
        color = 'black',
        linetype = 'dashed'
      ) + 
      ylab('Newly exposed individuals') +
      scale_x_continuous(
        name = NULL,
        limits = c(0, xmax),
        breaks = seq(0, xmax, 7),
        labels = (day0 + days(x_breaks)) %>% format("%m-%d")
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
    
    tots = apply(PREDS()$new_cases, 2, function(x) cumsum(x))/ MODEL()$N
    tots[tots>1] = 1
    tots[tots<0] = 0
    
    # Plot new admissions
    g2 = plot_admissions(
        tots, 
        future_days = input$future_days, 
        day0 = day0, 
        lag = 0,
        name='exposed individuals', 
        color='purple'
      ) +
      scale_y_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, 0.1),
        name = 'Total proportion of population exposed'
      ) +
      geom_vline(
        xintercept = full_social,
        color = 'black',
        linetype = 'dashed',
        size = 1
      ) + 
      geom_vline(
        xintercept = start_social,
        color = 'black',
        linetype = 'dashed',
        size = 1
      ) +
      scale_x_continuous(
        name = NULL,
        limits = c(0, xmax),
        breaks = seq(0, xmax, 7),
        labels = (day0 + days(x_breaks)) %>% format("%m-%d")
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust=1)
      )
    
    g = g1 | g2

    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 6, width=13)
    ggsave('plots/new_cases.png', height = 6, width=13)
    
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

    if (is.null(DF())) return(NULL)
    
    #--------------------------------------------------
    # Get the predictions
    # -------------------------------------------------
    df = DF()$df
    day0 = DF()$day0
    t0 = DF()$t0

    #--------------------------------------------------
    # Get the predictions
    # -------------------------------------------------
    full_social = lubridate::mdy('3/19/2020') 
    start_social = lubridate::mdy('3/12/2020') 
    x_breaks = seq(0, as.numeric(today()-day0+2), 7) 
    
    g <- ggplot() +
      geom_line(
        data = df,
        aes(x=as.numeric(date-day0), y=new_cases, linetype='observed'),
        color = 'orchid'
      ) +
      geom_point(
        data = df,
        aes(x=as.numeric(date-day0), y=new_cases),
        color = 'orchid'
      ) +
      geom_line(
        data = df,
        aes(x=as.numeric(lag_date-day0), y=new_cases , linetype='lag'),
        color = 'orchid'
      ) +
      geom_point(
        data = df,
        aes(x=as.numeric(lag_date-day0), y=new_cases),
        color = 'orchid'
      ) +
      geom_vline(
        xintercept = today() - day0,
        linetype='dashed',
        size = 1
      ) + 
      geom_vline(
        xintercept = start_social - day0,
        linetype='dashed',
        size = 1
      ) +  
      geom_vline(
        xintercept = mdy('3/19/2020') - day0,
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
        breaks = x_breaks,
        labels = (day0 + days(x_breaks)) %>% format("%m-%d"),
        limits = c(0, as.numeric(today()-day0+2)),
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        name = 'Count'
      ) +
      scale_linetype_manual(
        values = c('lag'='dashed', 'observed'='solid'),
        labels = c('lag'='Lag-adjusted data', 'observed'= 'Observed data')
      ) +
      theme_bw() +
      theme(
        legend.position = c(0.01, 0.99), 
        legend.justification = c(0, 1),
        legend.key.size = unit(1.5, "line"),
        legend.text = element_text(size=12),
        legend.title = element_blank(),
        legend.background = element_rect(fill = NA)
      )
    
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 6, width=13)
    ggsave('plots/observed_data.png', g, height = 6, width=13)
    
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

    # Get the data stuff
    df = DF()$df
    day0 = DF()$day0
    t0 = DF()$t0
    
    # Plot new admissions
    g1 = plot_admissions(
        PREDS()$hospital_admissions, 
        future_days = input$future_days, 
        day0 = day0, 
        lag = DEMAND_LAG,
        name=' COVID hospital admissions', 
        color='blue'
      ) + ggtitle('New COVID hospital admissions')
    
    
    # Plot new admissions
    g2 = plot_admissions(
      PREDS()$icu_admissions, 
      future_days = input$future_days, 
      day0 = day0, 
      lag = DEMAND_LAG,
      name=' COVID ICU patients', 
      color='firebrick'
    ) + ggtitle('New COVID ICU patients')
    
    # Plot new admissions
    g3 = plot_admissions(
      PREDS()$ventilator_admissions, 
      future_days = input$future_days, 
      day0 = day0, 
      lag = DEMAND_LAG,
      name=' COVID ventilator patients', 
      color='goldenrod'
    ) + ggtitle('New COVID ventilator patients')
    
    g = g1 | g2 | g3
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 5, width=13)
    ggsave('plots/new_demand.png', g, height = 5, width=13)
    
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
    
    # Get the data stuff
    df = DF()$df
    day0 = DF()$day0
    t0 = DF()$t0
    
    # Plot new admissions
    g1 = plot_demand(
      PREDS()$hospital_admissions, 
      los = input$hospital_los, 
      future_days = input$future_days, 
      day0 = day0, 
      lag = DEMAND_LAG,
      name=' COVID hospital patients', 
      color='blue'
    ) + ggtitle('Current total COVID hospital patients')
    
    
    # Plot new admissions
    g2 = plot_demand(
      PREDS()$icu_admissions, 
      los = input$icu_los, 
      future_days = input$future_days, 
      day0 = day0, 
      lag = DEMAND_LAG,
      name=' COVID ICU patients', 
      color='firebrick'
    ) + ggtitle('Current total COVID ICU patients')
    
    # Plot new admissions
    g3 = plot_demand(
      PREDS()$ventilator_admissions, 
      los = input$ventilator_los, 
      future_days = input$future_days,
      day0 = day0, 
      lag = DEMAND_LAG,
      name=' COVID ventilator patients', 
      color='goldenrod'
    ) + ggtitle('Current total COVID ventilator patients')
    
    g = g1 | g2 | g3
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 5, width=13)
    ggsave('plots/total_demand.png', g, height = 5, width=13)
    
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
        aes(y = stat(count) / sum(count))
      ) +
      scale_x_continuous(
        name = 'Doubling time'
      ) +
      scale_y_continuous(
        limits = c(0, 0.5),
        name = 'Proportion'
      ) +
      theme_bw() 
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 4, width=4)
    ggsave('plots/doubling_time.png', g, height = 4, width=4)
    
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
    ggsave('plots/contagious_time.png', g, height = 4, width=4)
    
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
  
  output$plot_R0 <- renderImage({
    
    samples = data.frame(
      doubling_time = PREDS()$samples$beta / PREDS()$samples$gamma
    ) 
    
    g <- ggplot(
        data = samples,
        aes(x=doubling_time)
      ) +
      geom_histogram(
        aes(y = stat(count) / sum(count))
      ) +
      scale_x_continuous(
        name = 'Estimated R0'
      ) +
      scale_y_continuous(
        limits = c(0, 0.5),
        name = 'Proportion'
      ) +
      theme_bw() 

    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 4, width=4)
    ggsave('plots/r0.png', g, height = 4, width=4)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 4*105,
      height = 4*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = FALSE)
  
  #---------------------------------------
  # Threshold tables
  #---------------------------------------
  
  output$hospital_thresholds = renderTable({
    
    # Make the tables
    tab = census_table(
      admissions=PREDS()$hospital_admissions, 
      los=input$hospital_los, 
      day0=DF()$day0, 
      lag = DEMAND_LAG,
      thresholds = c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)
    )
    
    write.csv(tab, 'plots/hospital_thresholds.csv')
    
    tab
  })
  
  output$icu_thresholds = renderTable({
    # Make the tables
    tab = census_table(
      admissions=PREDS()$icu_admissions, 
      los=input$icu_los, 
      day0=DF()$day0, 
      lag = DEMAND_LAG,
      thresholds = c(125, 250, 500, 1000, 2000, 4000, 8000, 16000)
    )
    
    write.csv(tab, 'plots/icu_thresholds.csv')
    
    tab
  })
  
  output$ventilator_thresholds = renderTable({
    # Make the tables
    tab = census_table(
      admissions=PREDS()$ventilator_admissions, 
      los=input$ventilator_los, 
      day0=DF()$day0, 
      lag = DEMAND_LAG,
      thresholds = c(100, 200, 400, 800, 1600, 3200, 6400, 12800)
    ) 
    write.csv(tab, 'plots/ventilator_thresholds.csv')
    
    tab
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
  
  output$est_r0_mean <- renderText({
    if (is.null(PREDS()$samples$beta)){
      return(NULL)
    }
    
    (PREDS()$samples$beta / PREDS()$samples$gamma)  %>% mean() %>% round(1) %>% as.character()
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
  
  output$est_r0_sd <- renderText({
    if (is.null(PREDS()$samples$beta)){
      return(NULL)
    }
    
    (PREDS()$samples$beta / PREDS()$samples$gamma)  %>% sd() %>% round(2) %>% as.character()
  })
  
  #---------------------------------------
  # Table of the data
  #---------------------------------------
  
  output$contents = renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame 
    return(DF()$df)
  })
  
  #---------------------------------------
  # Outdated
  #---------------------------------------
  
  output$icu <- renderImage({
    
    # Plot new admissions
    g1 = plot_admissions(
      PREDS()$icu_admissions, 
      future_days = input$future_days, 
      day0 = nrow(lax_data), 
      lag = DEMAND_LAG,
      name='ICU patients', 
      color='firebrick'
    )
    
    # Plot total census
    g2 = plot_demand(
      PREDS()$icu_admissions, 
      los = input$icu_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = DEMAND_LAG,
      name='ICU patients', 
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
      lag = DEMAND_LAG,
      name='hospital patients', 
      color='blue'
    )
    
    # Plot total census
    g2 = plot_demand(
      PREDS()$hospital_admissions, 
      los = input$hospital_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = DEMAND_LAG,
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
      lag = DEMAND_LAG,
      name='ventilator patients', 
      color='goldenrod'
    )
    
    # Plot total census
    g2 = plot_demand(
      PREDS()$ventilator_admissions, 
      los = input$ventilator_los, 
      future_days = input$future_days,
      day0 = nrow(lax_data), 
      lag = DEMAND_LAG,
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
  
}



