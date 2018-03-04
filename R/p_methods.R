p_methods <- function(data, pv_index, alpha){
  #
  ## A summary dataframe with columns for the p-values, adjusted p-values
  ## for both Bonferroni and Benjamini-Hochberg (BH), adjusted significance
  ## level for Bonferroni and the critical value for BH
  #
  ## Args:
  #     - data (dataframe): dataframe containing at least a column of p-values to be adjusted
  #     - pv_index (int): original p-value column index from existing input dataframe
  #     - alpha (numeric): significance level as a value between 0 and 1
  #
  ## Returns:
  #     Dataframe: appends to input dataframe both adjusted p-values and significance
  #                   levels (Bonferroni and BH) in ascending raw p-value order.
  #       Includes following columns:
  #       - bonf_val (numeric): Bonferroni adjusted significance level (same for all)
  #       - Bonf_significant (logical): True if significant p-value or False if not
  #       - bh_val (numeric): Benjamini-Hochberg (BH) critical value
  #       - BH_significant (logical): True if significant p-value or False if not
  #

}

