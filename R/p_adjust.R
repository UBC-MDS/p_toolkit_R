#' P-value Adjustments by Methods
#'
#' @usage p_adjust(data, pv_index, method, alpha = 0.05)
#'
#' @param data (dataframe) dataframe containing at least a column of p-values to be adjusted
#' @param pv_index (numeric) if the input is a dataframe, col refers to the column name of the dataframe that has the p-values
#' @param method (string) method used for the adjustment ("bh" or "bonf")
#' @param alpha (numeric) significance level for both methods
#'
#' @description
#' This function outputs a dataframe with the original p-values and their adjustments by the specified method.
#'
#' @details
#' Requirements:  Dplyr
#' 
#' @export
#' 
p_adjust <- function(data, pv_index, method= "bh", alpha=0.05){
  require(dplyr)
  if(is.data.frame(data)){
    ## check if all p-values are numeric
    #if(sapply(data[pv_index], is.numeric)){
      #data <- select(data, p_value = c(pv_index))
      #return(data)
    if(pv_index != "p_value"){
      df <- select(data, p_value = c(pv_index))
      data <- cbind(data, df)
      data <- select(data,-one_of(pv_index))
    }
    #}else{
      ## error for non-numeric data frame column
     # stop("Please ensure you have specified the column index of numeric p-values.")
    #}
    }else if(is.vector(data) ){#| is.matrix(data)){
      if(is.numeric(data)){
        ## convert matrix or vector to data frame
        data <- data.frame(p_value = data)
        #return(data)
      }else{
        ## error for non-numeric vector or matrix
        stop("Please ensure you input a vector or matrix of numeric p-values.")
      }
    }else{
        ## error for not being a data frame, matrix or vector
        stop("Please enter a data frame, matrix or vector of numeric p-values.")
    }

  ##Throw a warning if we have non valid probabilities
  if ((min(data$p_value)<0) | (max(data$p_value)>1)){
    warning("p-values should be between 0 and 1")
  }

  ### set the size of the data
  m = nrow(data)

  ### create the proper dataframe as a return
  data <- data %>%
    arrange(p_value) %>%
    mutate(
      bonf_pvalue = ifelse(p_value*m<1,p_value*m,1),
      # bonf_significant = ifelse(p_value<= bonf_val,TRUE,FALSE),
      rank = rank(p_value, ties.method= "min"),
      bh_pvalue = p_value*m/rank
    ) %>%
    select(-rank)

  if(method == 'bh' | method == 'fdr' |method == "BH"){
    return(data %>% select(-bonf_pvalue))
  }else if(method == 'bonf' | method == 'bonferroni'| method == "Bonferroni"){
    return(data %>% select(-bh_pvalue))
  }else{
    stop("Method should be specified as either 'Bonferroni' or 'BH'.")
  }

}
