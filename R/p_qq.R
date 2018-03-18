#' Multiple Comparison QQ-plot 
#'
#'
#' @usage p_qq(data, pv_index)
#' 
#' @param data (dataframe) dataframe containing at least a column of p-values to be adjusted
#' @param pv_index (numeric) if the input is a dataframe, col refers to the column name of the dataframe that has the p-values
#'
#' @description 
#' This function plots all the raw p-values and compares them with a theoretical uniform distribution using a
#' qq plot. This plot is created with a negative log scale, letting
#' us visualize all the p-values, independent of their small magnitudes. The p-values deviated from the diagonal line,
#' are the ones that are significant.
#'
#' @details
#' Requirements:  ggplot2
#' 
#' @return ggplot a ggplot object with the qq plot.
#' @export
#'
p_qq <- function(data, pv_index){
  require(dplyr)
  require(ggplot2)

  if(is.data.frame(data)){
    if(pv_index != "p_value"){
    ###change the pv_index column to p_value, in a dataframe
    df <- select(data, p_value = c(pv_index))
    data <- cbind(data, df)
    data <- select(data,-one_of(pv_index))
  }}

  else {
    ###if it's a vector, make it a dataframe of one column
    data <- data.frame(p_value = data)
  }

  ##Throw a warning if we have non valid probabilities
  if ((min(data$p_value)<0) | (max(data$p_value)>1)){
    warning("p-values should be between 0 and 1")
  }

  m <- length(data$p_value)
  #alpha <- data$value[1]

  data <- data %>%
    arrange(p_value) %>%
    mutate(log_transf = -log10(p_value),
           rank = row_number(),
           log_exp = -log10(rank/m))


  plot <- ggplot(data)+
    geom_point(aes(log_exp,log_transf))+
    geom_path(aes(log_exp,log_exp),color="red")+
    scale_y_continuous("Observed -log10(p)")+
    scale_x_continuous("Expected -log10(p)")+
    ggtitle("QQ")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  return(plot)
}
