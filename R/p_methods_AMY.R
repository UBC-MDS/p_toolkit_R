library(dplyr)



#' A summary dataframe with columns for the p-values, adjusted p-values
#' for both Bonferroni and Benjamini-Hochberg (BH), adjusted significance
#' level for Bonferroni and the critical value for BH
#'
#' Args:
#'     - data (dataframe): dataframe containing at least a column of p-values to be adjusted
#'     - pv_index (int): original p-value column index from existing input dataframe OR the name of the column in quotes
#'     - alpha (numeric): significance level as a value between 0 and 1
#'
#' Returns:
#'     Dataframe: appends to input dataframe both adjusted p-values and significance
#'                   levels (Bonferroni and BH) in ascending raw p-value order.
#'       Includes following columns:
#'       - bonf_value (numeric): Bonferroni adjusted significance level (same for all)
#'       - bonf_significant (logical): True if significant p-value or False if not
#'       - bh_value (numeric): Benjamini-Hochberg (BH) critical value
#'       - BH_significant (logical): True if significant p-value or False if not
#'
#' Requirements:  Dplyr

p_methods <- function(data, pv_index, alpha=0.05){
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


p_methods(toy_df, "p")



