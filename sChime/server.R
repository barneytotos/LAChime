source('../source/setup.R')
source('../source/models.R')
source('../source/utils.R')
source('../source/forecast.R')
library('patchwork')

#' Plots the new admissions/census for a service
#' @param preds: array, output of predict.model
#' @param rate: vector, the relative prevalence of the demand
#' @param los: int, length of stay
#' @param input: the input from ui
#' @param color: string, color to plot
#' @return ggplot of the new admissions/census for a single service.
plot_demand = function(preds, rate, los, input, color='blue'){
  # Predict admissions
  
  admissions = forecast_admissions_v(
    preds, 
    rate
  )
  print(dim(admissions))
  
  admissions_df = data.frame(
    ts = seq(1:input$future_days),
    lower  = apply(admissions, 1, quantile, probs=0.025),
    middle = apply(admissions, 1, quantile, probs=0.5),
    upper  = apply(admissions, 1, quantile, probs=0.975),
    typ = 'Hospital'
  )
  
  wrapper = function(s) data.frame(sim=s, ts = seq(1:input$future_days), ys=admissions[,s]) 
  inds = sample(1:input$sims, 25, replace = TRUE)
  admissions_sims = map(inds, wrapper) %>% bind_rows()
  
  # Predict census
  census = forecast_census_v(
    admissions, 
    rate,
    los
  )
  
  census_df = data.frame(
    ts = seq(1:input$future_days),
    lower  = apply(census, 1, quantile, probs=0.025),
    middle = apply(census, 1, quantile, probs=0.5),
    upper  = apply(census, 1, quantile, probs=0.975),
    typ = 'Hospital'
  )
  
  wrapper = function(s) data.frame(sim=s, ts = seq(1:input$future_days), ys=census[,s]) 
  census_sims = map(inds, wrapper) %>% bind_rows()
  
  # Plot of demand
  g1 = ggplot(
    ) +
    geom_line(
      data = admissions_df,
      aes(y=middle, x=ts), 
      show.legend = FALSE
    ) +
    geom_line(
      data = admissions_sims,
      aes(y=ys, x=ts, group=sim), 
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = admissions_df,
      aes(ymin=lower, ymax=upper, x=ts), 
      alpha=0.1,
      fill=color,
      show.legend = FALSE
    ) +
    theme_bw() +
    scale_x_continuous(
      name = 'Days from today'
    ) +
    scale_y_continuous(
      name = 'New admissions',
      limits = c(0, 10**3)*input$scaler,
      breaks = seq(0, 10**3, 100)*input$scaler
    )
  
  # Plot of census
  g2 = ggplot(
    )+
    geom_line(
      data = census_df %>% filter(ts > los),
      aes(y=middle, x=ts), 
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = census_df,
      aes(ymin=lower, ymax=upper, x=ts), 
      alpha=0.1,
      fill=color,
      show.legend = FALSE
    ) +
    geom_line(
      data = census_sims,
      aes(y=ys, x=ts, group=sim), 
      alpha = 0.1,
      show.legend = FALSE
    ) +
    theme_bw() +
    scale_x_continuous(
      name = 'Days from today'
    ) +
    scale_y_continuous(
      name = 'Demand census',
      limits = c(0, 10**4)*input$scaler,
      breaks = seq(0, 10**4, 1000)*input$scaler
    )
  
  g1 | g2
}

server <- function(input, output) {

  ########################################
  ## This runs the model
  ########################################
  PREDS = eventReactive(input$runButton, {
    
    set.seed(1337)
    
    # Sample the random parameters
    doubling_time = sample_gamma(
      input$doubling_time_mean,
      input$doubling_time_sd,
      input$sims
    )
    
    # Sample the random parameters
    recovery_time = sample_gamma(
      input$recovery_time_mean,
      input$recovery_time_sd,
      input$sims
    )
    
    # This is the hospitalization
    hospitalization_rate = sample_beta(
      input$hospitalization_mean, 
      input$hospitalization_weight,
      input$sims
    ) 
    
    # This is the hospitalization
    contact_reduction = sample_beta(
      input$contact_reduction_mean, 
      input$contact_reduction_weight,
      input$sims
    ) 
    

    # Initialise the stochastic SIR model
    model = StochasticSIR(
      N=input$N, 
      I0=input$I / input$market_share / hospitalization_rate, 
      doubling_time=doubling_time, 
      recovery_time=recovery_time
    )
    
    list(
      preds= predict(
        model, 
        last_time=input$future_days, 
        contact_reduction=contact_reduction
      ),
      hospitalization_rate = hospitalization_rate
    )

  })
  
  
  output$icu <- renderImage({
    
    # Get the things
    preds = PREDS()$preds
    rate = PREDS()$hospitalization_rate*input$market_share*input$icu_relative_rate
    g = plot_demand(preds, rate, input$icu_los, input, 'firebrick')
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 9, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 9*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$hospital <- renderImage({
    
    # Get the things
    preds = PREDS()$preds
    rate = PREDS()$hospitalization_rate*input$market_share
    g = plot_demand(preds, rate, input$hospitalization_los, input)
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 9, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 9*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
  output$ventilator <- renderImage({
    
    # Get the things
    preds = PREDS()$preds
    rate = PREDS()$hospitalization_rate*input$market_share*input$ventilator_relative_rate
    g = plot_demand(preds, rate, input$ventilator_los, input, 'goldenrod')
    
    # Save the file
    outfile <- tempfile(fileext = '.png')
    ggsave(outfile, g, height = 9, width=13)
    
    # Return a list containing the filename
    list(
      src = outfile,
      contentType = 'image/png',
      width = 13*105,
      height = 9*105,
      alt = "This is alternate text"
    )
  }, 
  deleteFile = TRUE)
  
}



