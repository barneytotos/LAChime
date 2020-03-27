

#' Helper function that converts the output
#' Of a preds object and turns it into a longer-dataframe
#' for plotting
#' @param preds: array of dim (times, 2, sims)
#' @return data.frame with columnes t, sim, infected, delta
pred_to_df <- function(preds){
  
  # Get the dimensions
  times = 1:dim(preds)[1]
  sims = dim(preds)[3]
  
  # Convenienve function
  wrapper = function(s) data.frame(t=times, sim=s, infected=preds[,3,s], delta=preds[,4,s])
  map(1:sims, wrapper) %>% bind_rows()
  
}
