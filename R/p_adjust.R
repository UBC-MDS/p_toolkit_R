p_adjust <- function(data, pv_index, alpha){
  #
  # This function executes an specific p-value adjustment method.
  #
  ## Args:
  #   -  data (dataframe): dataframe containing at least a column of p-values to be adjusted
  #   - col (int): if the input is a dataframe, col refers to the column name of the dataframe that has the p-values.
  #   - alpha (numeric): significance level for both methods.
  #   - method (string): method used for the adjustment ("bh" or "bonf")
  #
  ## Returns:
  #   - data frame: data frame with the following columns:
  #       raw_p_value (numeric): original p-values.
  #       adjusted_p_value (numeric): p-values after the adjustment.
  #       signficant (logical): boolean values determining if each p-value is significant.
  #       critical_value (numeric): it's the calculated critical value to compare with the cut-off.

}


