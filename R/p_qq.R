library(ggplot)
library(dplyr)

p_qq <- function(ad_object){
  # This function plots all the raw p-values and compares them with a theoretical uniform distribution using a
  # qq plot. This plot is created with a negative log scale, letting
  # us visualize all the p-values, independent of their small magnitudes. The p-values deviated from the diagonal line,
  # are the ones that are significant.
  #
  # Args:
  #   - ad_object: the dataframe output from the p_methods function.
  #
  # Returns:
  #   - plot: a ggplot object with the qq plot.
}

