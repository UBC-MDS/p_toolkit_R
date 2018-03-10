#' This function plots all the raw p-values and compares them with a theoretical uniform distribution using a
#' qq plot. This plot is created with a negative log scale, letting
#' us visualize all the p-values, independent of their small magnitudes. The p-values deviated from the diagonal line,
#' are the ones that are significant.
#'
#' Args:
#'   - ad_object: the dataframe output from the p_methods function.
#'
#' Returns:
#'   - plot: a ggplot object with the qq plot.


#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples

p_qq <- function(data, pv_index){
  require(dplyr)
  require(ggplot2)

  if(is.data.frame(data)){
    ###change the pv_index column to p_value, in a dataframe
    df <- select(data, p_value = c(pv_index))
    df <- cbind(data, df)
    df <- select(df,-one_of(pv_index))
  }

  else {
    ###if it's a vector, make it a dataframe of one column
    df <- data.frame(p_value = data)
  }

  data <- data %>%
    arrange(p_value) %>%
    mutate(log_transf = -log10(p_value),
           rank = row_number(),
           log_exp = -log10(rank/length(data$rank)))

  m <- length(data$p_value)
  alpha <- data$value[1]

  plot <- ggplot(data)+
    geom_point(aes(log_exp,log_transf))+
    geom_path(aes(log_exp,log_exp),color="red")+
    scale_y_continuous("Observed -log10(p)")+
    scale_x_continuous("Expected -log10(p)")+
    ggtitle("QQ")+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5))
  return(list(plot))
}
