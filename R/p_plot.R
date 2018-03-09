library(ggplot)
library(dplyr)

p_plot <- function(ad_object){
  # This function plots all the p-values in ascending order and compares them with two lines, one representing
  # the BH cutoff point and another one the Bonferroni cutoff.
  #
  # Args:
  #   - ad_object: the dataframe output from the p_methods function.
  #
  # Returns:
  #   - plot: a ggplot with the p-values and both cut-off lines.

  data <- data %>%
    arrange(p_value) %>%
    mutate(rank = row_number())

  m <- length(data$p_value)
  alpha <- data$value[1]
}

