BLUE = "DODGERBLUE"
ORANGE = "#E69F00"
PURPLE = "#9933FF"
PINK = "orchid"
GREY = "grey80"
DARKGRAY = 'gray35'


#' Plots the new admissions/census for a service
#' @param preds: array, output of predict.model
#' @param input: the input from ui
#' @param color: string, color to plot
#' @return ggplot of the new admissions/census for a single service.
plot_admissions = function(admissions, future_days, day0, lag=0, name='', color='blue'){
  
  # Set the times scale
  ts = 1:dim(admissions)[1]
  
  first_day = (today() - days(14)) - day0 
  xmin = as.numeric(first_day)
  xmax = as.numeric((today() + days(future_days)) - day0)
  x_breaks = seq(xmin, xmax, 7)
  
  # Admissions uncertainty
  admissions_df = data.frame(
    ts = ts,
    lower  = apply(admissions, 1, quantile, probs=0.025),
    middle = apply(admissions, 1, quantile, probs=0.5),
    upper  = apply(admissions, 1, quantile, probs=0.975)
  )
  
  # Admissions samples
  wrapper = function(s) data.frame(sim=s, ts = ts, ys=admissions[,s]) 
  inds = sample(1000, 25, replace = FALSE)
  admissions_sims = map(inds, wrapper) %>% bind_rows()

  # Setup the y axis
  ymax = admissions_df %>% 
    filter(ts <= xmax) %>% 
    pull(upper) %>% 
    max() %>% 
    smart_max()
  
  # Plot of demand
  ggplot() +
    geom_line(
      data = admissions_df %>% filter(ts<=xmax),
      aes(y=middle, x=ts), 
      show.legend = FALSE
    ) +
    geom_line(
      data = admissions_sims %>% filter(ts<=xmax),
      aes(y=ys, x=ts, group=sim), 
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_ribbon(
      data = admissions_df %>% filter(ts<=xmax),
      aes(ymin=lower, ymax=upper, x=ts), 
      alpha=0.1,
      fill=color,
      show.legend = FALSE
    ) +
    geom_vline(xintercept = today()-day0, color='black', linetype='dashed') + 
    theme_bw() +
    scale_x_continuous(
      name = NULL,
      limits = c(xmin, xmax),
      breaks = seq(xmin, xmax, 7),
      labels = (day0 + days(x_breaks)) %>% format("%m-%d")
    ) +
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = seq(0, ymax, length.out = 11),
      name = sprintf('New %s', name)
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
}


#' Plots the new admissions/census for a service
#' @param preds: array, output of predict.model
#' @param rate: vector, the relative prevalence of the demand
#' @param los: int, length of stay
#' @param input: the input from ui
#' @param color: string, color to plot
#' @return ggplot of the new admissions/census for a single service.
plot_demand = function(admissions, los, future_days, day0, lag='', name='', color='blue'){

  # Set the times scale
  ts = 1:dim(admissions)[1]
  
  first_day = (today() - days(14)) - day0 
  xmin = as.numeric(first_day)
  xmax = as.numeric((today() + days(future_days)) - day0)
  x_breaks = seq(xmin, xmax, 7)
  
  
  # Create the census
  census = forecast_census_v(
    admissions, 
    los
  )

  # Census uncertainty
  census_df = data.frame(
    ts = ts,
    lower  = apply(census, 1, quantile, probs=0.025),
    middle = apply(census, 1, quantile, probs=0.5),
    upper  = apply(census, 1, quantile, probs=0.975)
  )
  
  # Census samples
  wrapper = function(s) data.frame(sim=s, ts = ts, ys=census[,s]) 
  inds = sample(1000, 25, replace = FALSE)
  census_sims = map(inds, wrapper) %>% bind_rows()
  
  # Setup the y axis
  ymax = census_df %>% 
    filter(ts <= xmax) %>% 
    pull(upper) %>% 
    max() %>% 
    smart_max()
  
  # Plot of census
  ggplot()+
    geom_ribbon(
      data = census_df %>% filter(ts<=xmax, ts>=xmin),
      aes(ymin=lower, ymax=upper, x=ts), 
      alpha=0.1,
      fill=color,
      show.legend = FALSE
    ) +
    geom_line(
      data = census_sims %>% filter(ts<=xmax, ts>=xmin),
      aes(y=ys, x=ts, group=sim), 
      alpha = 0.1,
      show.legend = FALSE
    ) +
    geom_line(
      data = census_df %>% filter(ts > los) %>% filter(ts<=xmax, ts>=xmin),
      aes(y=middle, x=ts), 
      show.legend = FALSE
    ) +
    geom_vline(xintercept = today()-day0, color='black', linetype='dashed') + 
    theme_bw() +
    scale_x_continuous(
      name = NULL,
      limits = c(xmin, xmax),
      breaks = seq(xmin, xmax, 7),
      labels = (day0 + days(x_breaks)) %>% format("%m-%d")
    ) +
    # TODO dynamic scaling
    scale_y_continuous(
      limits = c(0, ymax),
      breaks = seq(0, ymax, length.out = 11),
      name = sprintf('Total current %s', name)
    )  +
    theme(
      axis.text.x = element_text(angle = 45, hjust=1)
    )
}


census_table = function(admissions, los, day0, thresholds){
  
  # Create the census
  inds = as.numeric((today() + days(c(7, 14, 21, 28))) - day0)
  
  census = forecast_census_v(
    admissions, 
    los
  )[inds, ]
  
  
  tabl = data.frame(
    threshold = thresholds %>% format(big.mark=",", trim=TRUE),
    apply(census, 1, function(x) outer(x, thresholds, '>') %>% colMeans() %>% sprintf(' %.2f ', .)) 
  )
  
  colnames(tabl) <- c('Threshold', '7 days', '14 days', '21 days', '28 days')
  
  return(tabl)
}



smart_max = function(ymax){
  magnitude = floor(log10(ymax*1.2))
  ceiling(ymax*1.2 / 10**magnitude)*10**magnitude
}



addTitleGrob <- function(table_grob, title, fontsize = 16){
  title_grob <- textGrob(title, gp = gpar(fontsize = fontsize))
  ## Add title
  table_grob <- gtable_add_rows(table_grob, heights = grobHeight(title_grob) + unit(5,'mm'), pos = 0)
  table_grob <- gtable_add_grob(table_grob, title_grob, 1, 1, 1, ncol(table_grob), clip = "off")
  
  return(table_grob)
}

