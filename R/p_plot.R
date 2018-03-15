#' This function plots all the p-values in ascending order and compares them with two lines, one representing
#' the BH cutoff point and another one the Bonferroni cutoff.
#'
#' Args:
#'   - ad_object: the dataframe output from the p_methods function.
#'
#' Returns:
#'   - plot: a ggplot with the p-values and both cut-off lines.

#' Title
#'
#' @param data
#'
#' @return ggplot2
#' @export
#'

p_plot <- function(data, pv_index,alpha = 0.05){
  require(dplyr)
  require(ggplot2)

  if(is.data.frame(data)){
    ###change the pv_index column to p_value, in a dataframe
    df <- select(data, p_value = c(pv_index))
    data <- cbind(data, df)
    data <- select(data,-one_of(pv_index))
  }

  else {
    ###if it's a vector, make it a dataframe of one column
    data <- data.frame(p_value = data)
  }

  ##Throw a warning if we have non valid probabilities
  if ((min(data$p_value)<0) | (max(data$p_value)>1)){
    warning("p-values should be between 0 and 1")
  }

  data <- data %>%
    arrange(p_value) %>%
    mutate(rank = row_number())

  m <- length(data$p_value)
  #alpha <- data$value[1]

  plot <- ggplot(data)+
    geom_point(aes(rank,p_value))+
    geom_abline(slope = 0,intercept = alpha,color="red")+
    geom_abline(slope = alpha/m,intercept = 0,color="blue")+
    scale_y_continuous("p(k)",limits = c(0,0.5))+
    scale_x_continuous("Rank")+
    ggtitle("Bonferroni vs BH")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}
