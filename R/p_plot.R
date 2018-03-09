library(ggplot)
library(dplyr)

p_plot <- function(data){
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

  plot <- ggplot(data)+
    geom_point(aes(rank,p_value))+
    geom_abline(slope = 0,intercept = alpha,color="red")+
    geom_abline(slope = alpha/m,intercept = 0,color="blue")+
    scale_y_continuous("p(k)",limits = c(0,0.5))+
    scale_x_continuous("Rank")+
    ggtitle("Bonferroni vs BH")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  return(list(plot))
}

