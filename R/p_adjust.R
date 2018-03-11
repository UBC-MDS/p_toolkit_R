#' P-value Adjustments
#' This function executes an specific p-value adjustment method.
#' Args:
#' @param data (dataframe): dataframe containing at least a column of p-values to be adjusted
#' @param pv_index (int): if the input is a dataframe, col refers to the column name of the dataframe that has the p-values.
#' @param method (numeric): significance level for both methods.
#' @param alpha (string): method used for the adjustment ("bh" or "bonf")
#'
#' @return
#' @export
#'
#'Returns:
#'   - data frame: data frame with the following columns:
#' raw_p_value (numeric): original p-values.
#'       adjusted_p_value (numeric): p-values after the adjustment.
#'       signficant (logical): boolean values determining if each p-value is significant.
#'       critical_value (numeric): it's the calculated critical value to compare with the cut-off.
#'
#' @examples
#'
#' # Trial
#'
#'
#nSims <- 100 #number of simulated experiments
#p <-numeric(nSims) #set up empty container for all simulated p-values

#for(i in 1:nSims){ #for each simulated experiment
 # x<-rnorm(n = 100, mean = 10, sd = 8) #produce 100 simulated participants
  #with mean=10 and SD=8
 # y<-rnorm(n = 100, mean = 30, sd = 5) #produce 100 simulated participants
  #with mean=30 and SD=5
 # z<-t.test(x,y) #perform the t-test
  #p[i]<-z$p.value #get the p-value and store it
#}
#p_adjust(p ,1, "bonf",0.05)
#'
#'


p_adjust <- function(data, pv_index, method, alpha=0.05){
  require(dplyr)
  if(is.data.frame(data)){
    ## check if all p-values are numeric
    #if(sapply(data[pv_index], is.numeric)){
      #data <- select(data, p_value = c(pv_index))
      #return(data)
      df <- select(data, p_value = c(pv_index))
      data <- cbind(data, df)
      data <- select(data,-one_of(pv_index))

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
    return(sprintf("Method should be set as 'bonf' or 'bh' corrections"))
  }

}


