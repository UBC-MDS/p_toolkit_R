#' P-value Summary Plot
#' 
#' @description
#' This function plots all the p-values in ascending order and compares them with two lines, one representing the BH cutoff point and another one the Bonferroni cutoff.
#' 
#' @usage p_plot(data, pv_index, alpha = 0.05)
#'
#' @param data dataframe containing at least a column of p-values to be adjusted
#' @param pv_index (numeric) if the input is a dataframe, col refers to the column name of the dataframe that has the p-values
#' @param alpha (numeric) significance level for both methods
#'
#' @return ggplot dataframe output from the p_methods function
#'
#' @export
#'
p_plot <- function(data, pv_index,alpha = 0.05){
  require(dplyr)
  require(ggplot2)

  if(is.data.frame(data)){
    ###change the pv_index column to p_value, in a dataframe
    if(pv_index != "p_value"){
      df <- select(data, p_value = c(pv_index))
      data <- cbind(data, df)
      data <- select(data,-one_of(pv_index))
    }
  }

  else {
    ###if it's a vector, make it a dataframe of one column
    data <- data.frame(p_value = data)
  }

  ##Throw a warning if we have non valid probabilities
  if ((min(data$p_value)<0) | (max(data$p_value)>1)){
    warning("p-values should be between 0 and 1")
  }
  ##Throw a warning if alpha is a non valid probability
  if ((alpha<=0) | (alpha>=1)){
    warning("alpha should be between 0 and 1")
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
