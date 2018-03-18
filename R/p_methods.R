#' P-value Corrections
#'
#' @usage p_methods(data, pv_index, alpha = 0.05)
#'
#' @param data (dataframe) dataframe containing at least a column of p-values to be adjusted
#' @param pv_index (numeric) if the input is a dataframe, col refers to the column name of the dataframe that has the p-values
#' @param alpha (numeric) significance level for both methods
#'
#' @description Dataframe: appends to input dataframe both adjusted p-values and significance
#'                  levels (Bonferroni and BH) in ascending raw p-value order.
#'      Includes following columns:
#'      - bonf_value (numeric): Bonferroni adjusted significance level (same for all)
#'      - bonf_significant (logical): True if significant p-value or False if not
#'      - bh_value (numeric): Benjamini-Hochberg (BH) critical value
#'      - BH_significant (logical): True if significant p-value or False if not
#'
#' @details
#' Requirements:  Dplyr
#'
#' @details
#' Requirements:  Dplyr
#'
#' @examples
#' # test function
#' p_method(data = [0.03,0.05,0.08,0.2], pv_index = 1, alpha = 0.05)
#' @return data.frame
#' @export
#'
p_methods <- function(data, pv_index=1, alpha=0.05){
  require(dplyr)

  #Type checking
  if (is.null(data)){
    stop("Data is missing")
  }else if(is.null(pv_index)){
    stop("Column containing the p value is missing")
  #}else if(!is.numeric(pv_index)){
   # stop("Pv_index is not numeric")
  }else if(!is.numeric(alpha)){
    stop("Alpha is not numeric")
  }


  if(is.data.frame(data)){
    ###change the pv_index column to p_value, in a dataframe
    if(pv_index != "p_value"){
      df <- select(data, p_value = c(pv_index))
      df <- cbind(data, df)
      df <- select(df,-one_of(pv_index))
    }
    else {
      df = data
      }
  }

  else {
    ###if it's a vector, make it a dataframe of one column
    df <- data.frame(p_value = data)
  }

  ##Throw a warning if we have non valid probabilities
  if ((min(df$p_value)<0) | (max(df$p_value)>1)){
    warning("p-values should be between 0 and 1")
  }

  ##Throw a warning if alpha is a non valid probability
  if ((alpha<=0) | (alpha>=1)){
    warning("alpha should be between 0 and 1")
  }

  ### set the size of the data
  m <-  nrow(df)

  ###find the smallest p_value s.t p<k*alpha/m (BH method)
  max_true <- df %>%
    mutate(
      rank = rank(p_value, ties.method= "min"),
      bh_val = alpha*rank/m,
      bh_sig = ifelse(p_value<= bh_val,TRUE,FALSE)
    ) %>%
    filter(bh_sig) %>%
    pull(rank) %>%
    max(0)

  ### create the proper dataframe as a return
  df <- df %>%
    arrange(p_value) %>%
    mutate(
      bonf_value = alpha/m,
      bonf_significant = ifelse(p_value<= bonf_value,TRUE,FALSE),
      rank = rank(p_value, ties.method= "min"),
      bh_value = alpha*rank/m,
      bh_significant = ifelse(rank<= max_true,TRUE,FALSE)
    ) %>%
    select(-rank)

  return(df)
}

p_methods(c(0.07, 0.2),1)
