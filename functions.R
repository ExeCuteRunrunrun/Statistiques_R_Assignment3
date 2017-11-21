# Difference in the proportion of cases with a specific value between two groups.
#
# ARGUMENTS:
# d: a data frame or tibble
# var: the name of a column of d containing the dependent variable, provided as a string
# grouping_var: the name of a column of d containing a grouping variable, provided as a string
# group1: the value of grouping_var that corresponds to the first group
# group2: the value of grouping_var that corresponds to the second group
# target_value: the value of var that will be counted
#
# RETURN VALUE:
# The percentage of cases in which `var` was equal to `target_value` for the first group,
# minus the percentage of cases in which `var` was equal to `target_value` for the
# second group.
#
difference_in_proportion <- function(d, var, grouping_var, group1, group2,
                                     target_value) {
  # (stress, Syllable, Category, Noun, Verb, "Syllable 1")
  # YOUR CODE HERE: calculate the proportion of cases in which `var` is equal to
  # the value specified in `target_value` in the first group, and then again in the
  # second group
  # YOUR CODE HERE: assign the difference in these proportions to the variable `result`
  ns_1 <- sum(d[,var]==target_value&d[,grouping_var]==group1)
  ns_2 <- sum(d[,var]==target_value&d[,grouping_var]==group2)
  
  n_1 <- sum(d[,grouping_var]==group1)
  n_2 <- sum(d[[grouping_var]]==group2)
  result <- ns_1/n_1-ns_2/n_2
  return(result)
}

randomize <- function(d, var) {
  d[[var]] <- sample(d[[var]], nrow(d)) # generate a shuffled version of d[[var]]
  
  return(d)
}

permutation_twogroups <- function(d, var, grouping_var, group1, group2, statistic,
                                  n_samples=9999,...) {
  observed_statistic <- statistic(d, var, grouping_var, group1, group2,...) # the real statistic data that we observed
  # In this case of our assignment, it's the real median difference observed
  permutation_statistics <- rep(0, n_samples) # replicate 0 as many as n_samples
  # In fact, this step could be seen as "initialization of variable with indicating its type: interger"
  for (i in 1:n_samples) { 
    # Here's a specific usage of "i" in R, in Python list[0] is actually [1] in R
    # YOUR CODE HERE: use randomize(...) to create a permutation and then
    # fill in the vector permutation_statistics with the
    # value of statistic(...) for this new permutation
    
    d_fake <- randomize(d,var) # randomize the column var to generate fake data
    #d_fake <- randomize(d,grouping_var)
    #d_fake <- d
    #print(d_fake)
    
    permutation_statistics[i] <- statistic(d_fake, var, grouping_var, group1, group2,...) # get the i th fake statistic data of permuted data
  }
  result <- list(observed=observed_statistic,
                 permuted=permutation_statistics)
  return(result) # result contains "observed" and "permuted" which will be useful after
}

permutation_pvalue_right <- function(p) {
  n_above <- sum(p$permuted >= p$observed)
  n_samples <- length(p$permuted)
  return((n_above + 1)/(n_samples + 1))
}

# Perform a permutation test for two implicit groups of binary trials summarized
# by the number of "successes" and trials for each of the two groups, using the
# difference in the proportion of successes (group 1 minus group 2) as a test
# statistic.
#
# ARGUMENTS:
# k1: the number of "successes" (i.e., observations of one of the two types) in group 1
# k2: the number of "successes" in group 2
# n1: the total number of trials in group 1
# n2: the total number of trials in group 2
# n_samples: the number of permutations (defaults to 9999)
#
# RETURN VALUE:
# 
# A list containing two elements:
#
#  - observed: the value of statistic() in d
#  - permuted: a vector containing the values of statistic() under n_samples
#              permutations
#
permtest_difference_in_props <- function(k1, k2, n1, n2, n_samples=9999) {
  # Create a set of observations with exactly k1 and k2 successes
  obs_1 <- c(rep(TRUE, k1), rep(FALSE, n1 - k1)) # Individual observations from group 1
  obs_2 <- c(rep(TRUE, k2), rep(FALSE, n2 - k2)) # Individual observations from group 2
  observations <- c(obs_1, obs_2)
  
  # Permute this set of observations n_samples times, saving the result in a
  # matrix
  rep_observations <- matrix(rep(observations, n_samples), n1 + n2)
  perm_observations <- apply(rep_observations, 2, sample, n1 + n2)
  
  # Generate the proportions in the two groups amongst the permuted observations.
  # Tricks: mean() of a TRUE/FALSE variable is the proportion "TRUE";
  # instead of having explicit "Group" labels that we hold fixed, we just hold fixed
  # that the first n1 rows are "Group 1" and the remaining n2 rows are "Group 2",
  # which amounts to the same thing, and we generate the two percentages directly.
  props_group1 <- colMeans(perm_observations[1:n1,])
  props_group2 <- colMeans(perm_observations[(n1+1):(n1+n2),])
  
  test_stats <- props_group1 - props_group2
  return(list(observed=((k1/n1) - (k2/n2)), permuted=test_stats))
}
v_pdp_pvalue_right <- function(k1_vec, k2_vec, n1, n2, n_samples=9999) {
  result <- rep(NA, length(k1_vec))
  for (i in 1:length(k1_vec)) {
    # [YOUR CODE HERE: APPLY permtest_difference_in_props WITH THE i'TH VALUES
    #  OF k1_vec AND OF k2_vec AS THE FIRST TWO ARGUMENTS, AND STORE THE
    #  RESULT AS THE i'TH VALUE OF result]
    p <- permtest_difference_in_props(k1_vec[i],k2_vec[i],n1,n2,n_samples = 9999)
    result[i] <- permutation_pvalue_right(p)
    #result[i] <- p
  }
  return(result)
}