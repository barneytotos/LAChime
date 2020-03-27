source('../source/setup.R')
source('../source/models.R')
source('../source/utils.R')
source('../source/forecast.R')


server <- function(input, output) {

  
  output$demand <- renderImage({

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
    
    # Thing
    admits = forecast_admissions(preds, input$rate_hospitalized * input$market_share)
    census = forecast_census(preds, input$rate_hospitalized * input$market_share, input$length_of_stay)

    # Make a data frame
    forecast_data = data.frame(
      ts = 1:input$future_days,
      admits = admits
    )
    
    # Make a plot
    g = ggplot(
        data=forecast_data,
        aes(x=ts, y=admits)
      ) +
      geom_line(color='firebrick') +
      geom_point(color='firebrick') +
      scale_x_continuous(
        name = 'Days from today'
      ) +
      scale_y_continuous(
        name = 'New admissions'
      ) +
      theme_bw()
    g
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 7.5, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 7.5*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$census <- renderImage({
    
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
    
    # Thing
    admits = forecast_admissions(preds, input$rate_hospitalized * input$market_share)
    census = forecast_census(preds, input$rate_hospitalized * input$market_share, input$length_of_stay)
    
    # Make a data frame
    forecast_data = data.frame(
      ts = 1:input$future_days,
      admits = census
    )
    
    # Make a plot
    g = ggplot(
        data=forecast_data,
        aes(x=ts, y=admits)
      ) +
      geom_line(color='firebrick') +
      geom_point(color='firebrick') +
      scale_x_continuous(
        name = 'Days from today'
      ) +
      scale_y_continuous(
        name = 'Currently admitted patients'
      ) +
      theme_bw()
    g
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 7.5, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 7.5*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  
}



