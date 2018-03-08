library(dplyr)

#
## A summary dataframe with columns for the p-values, adjusted p-values
## for both Bonferroni and Benjamini-Hochberg (BH), adjusted significance
## level for Bonferroni and the critical value for BH
#
## Args:
#     - data (dataframe): dataframe containing at least a column of p-values to be adjusted
#     - pv_index (int): original p-value column index from existing input dataframe OR the name of the column in quotes
#     - alpha (numeric): significance level as a value between 0 and 1
#
## Returns:
#     Dataframe: appends to input dataframe both adjusted p-values and significance
#                   levels (Bonferroni and BH) in ascending raw p-value order.
#       Includes following columns:
#       - bonf_val (numeric): Bonferroni adjusted significance level (same for all)
#       - bonf_significant (logical): True if significant p-value or False if not
#       - bh_val (numeric): Benjamini-Hochberg (BH) critical value
#       - BH_significant (logical): True if significant p-value or False if not
#
## Requirements:  Dplyr

p_methods <- function(data, pv_index, alpha=0.05){
  if(is.data.frame(data)){
    ###change the pv_index column to p_value, in a dataframe
    data <- select(data, p_value = c(pv_index))
  }
  if (is.vector(data)){
    ###if it's a vector, make it a dataframe of one column
    data <- data.frame(p_value = data)
  }
  ### set the size of the data
  m = nrow(data)
  
  ###find the smallest p_value s.t p<k*alpha/m (BH method)
  max_true <- data %>%
    mutate(
      rank = rank(p_value, ties.method= "min"),
      bh_val = alpha*rank/m,
      bh_sig = ifelse(p_value<= bh_val,TRUE,FALSE)
    ) %>%
    filter(bh_sig) %>%
    pull(rank) %>%
    max()

  ### create the proper dataframe as a return
  data <- data %>%
    arrange(p_value) %>%
    mutate(
      bonf_val = alpha/m,
      bonf_significant = ifelse(p_value<= bonf_val,TRUE,FALSE),
      rank = rank(p_value, ties.method= "min"),
      bh_val = alpha*rank/m,
      bh_significant = ifelse(rank<= max_true,TRUE,FALSE)
    ) %>%
    select(-rank)
  return(data)
}

p_methods(amy)
p_methods(toy_df, 2)



amy


alpha = 0.05
m = 5
data.frame(p_value = amy) %>%
  mutate(
    rank = rank(p_value, ties.method= "min"),
    bh_val = alpha*rank/m,
    bh_sig = ifelse(p_value<= bh_val,TRUE,FALSE)
  )%>%
  filter(bh_sig) %>%
  pull(rank) %>%
  max()




pretend <- function(data, p_value){
  data <- data %>% 
    select(c(p_value))
  return(data)
}

pretend(toy_df, "p")


toy_df <- data.frame(test= c("test1", "test2"), p = c(.1,.05))

amy <- c(.01,.2,.03, .03, .035, .03)

rank(amy, ties.method="min")

is.vector(amy)

mike <- pp(amy)

mike

pp(12)

is.vector


pp(mike)

