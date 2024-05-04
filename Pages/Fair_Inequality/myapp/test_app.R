
library(shiny)
library(comprehenr)
library(tidyverse)
library(EnvStats)
library(ggplot2)



sib_n_in_group = 10
sib_phi = 0.6

sib_mean_income_slider = c(50, 50)
sib_var_men = 0.9
sib_var_women = 0.5

#=========================================================================#
# DATA
#=========================================================================#


n_indivs = sib_n_in_group
sib_phi = sib_phi

# Generate original incomes
incomes_man <- pmax(rlnorm(n_indivs, log(sib_mean_income_slider[2]), sib_var_men), 0)
incomes_woman <- pmax(rlnorm(n_indivs, log(sib_mean_income_slider[1]), sib_var_women), 0)

group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
group_man <- to_vec(for(i in 1:n_indivs) "Man")


# Generate random element of the SSS income
incomes_man_sss_random <- pmax(rlnorm(n_indivs, log(sib_mean_income_slider[2]), sib_var_men), 0)
incomes_woman_sss_random <- pmax(rlnorm(n_indivs, log(sib_mean_income_slider[1]), sib_var_women), 0)

# Combine the two to get the SSS income
incomes_man_sss = sib_phi * incomes_man_sss_random + (1-sib_phi) * incomes_man
incomes_woman_sss = sib_phi * incomes_woman_sss_random + (1-sib_phi) * incomes_woman
  



# If siblings, need to have some extra columns to make it work
df <- data.frame(c(incomes_man, incomes_woman), c(incomes_man_sss, incomes_woman_sss), c(group_man, group_woman))
colnames(df) <- c("Income", "Income_SSS", "Group")


df

#=========================================================================#
# FUNCTIONS
#=========================================================================#

calc_total_gini <- function(dataframe){
  # Get incomes
  incomes = dataframe$Income
  n = length(incomes)
  
  # Need to divide and multiply to divide by the right amount and get the correct mean
  mad = sum(as.numeric(dist(incomes))) / (0.5 * n^2)
  rmad = mad / mean(incomes)
  gini = 0.5 * rmad * n/(n-1)
  
  # Get all possible pairs
  return(gini)
}

# Calculates Gini within a given group
calc_group_gini <- function(dataframe){
  df_gini <- data.frame(matrix(ncol = 2, nrow = 0))
  colnames(df_gini) <- c("Group", "Gini")
  
  # Calculate total Gini and add
  total_gini <- calc_total_gini(dataframe)
  df_gini[nrow(df_gini) + 1,] = list(Group = "All", Gini = total_gini)
  
  # Iterate over groups
  for(group in unique(dataframe$Group)){
    # Calculate group Gini
    df_group <- df %>% filter(Group == group)
    group_gini <- calc_total_gini(df_group)
    # Add to group
    df_gini[nrow(df_gini) + 1,] = list(Group = group, Gini = group_gini)
  }
  
  
  return(df_gini)
}


#=========================================================================#
# WIP
#=========================================================================#


calc_sib_gini <- function(dataframe) {
  
  ## Calculate Gini for entire population
  # Reshape such that there is only one income column
  dataframe_long <- dataframe %>%
    pivot_longer(cols = starts_with("Income"),
                 names_to = "Sibling",
                 values_to = "Income")
  
  # Within sibling
  n = length(dataframe$Income)
  
  mad = sum(abs(dataframe$Income - dataframe$Income_SSS)) / n
  rmad = mad / mean(dataframe_long$Income)
  gini = 0.5 * rmad * n/(n-1)
  
  return(dataframe_long)
  
}

# Sibling Gini for each group
calc_sib_groub_gini <- function(dataframe) {
  
  # Setup table
  df_gini <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(df_gini) <- c("Group", "Within_SSS", "Total")
  
  # Need long version of data
  dataframe_long <- dataframe %>%
    pivot_longer(cols = starts_with("Income"),
                 names_to = "Sibling",
                 values_to = "Income")
  
  # Aggregates are easy
  all_within_SSS_gini = calc_sib_gini(dataframe)
  all_total_gini <- calc_total_gini(dataframe_long)
  
  # Fill in the All category
  df_gini[nrow(df_gini) + 1,] = list(Group = "All", Within_SSS = all_within_SSS_gini, Total = all_total_gini)
  
  # Iterate over groups
  for(group in unique(dataframe$Group)){
    # Calculate group Gini and sibling gini
    df_group <- df %>% filter(Group == group)
    
    group_gini <- calc_total_gini(df_group)
    group_sib_gini <- calc_sib_gini(df_group)
    
    
    # Add to group
    df_gini[nrow(df_gini) + 1,] = list(Group = group, Within_SSS = group_sib_gini, Total = group_gini)
  }
  
  return(df_gini)
}


print(calc_sib_gini(df), n=50)


# Testing for it

calc_total_gini <- function(inc_vector){
  # Get incomes
  
  incomes = inc_vector
  n = length(incomes)
  
  # Need to divide and multiply to divide by the right amount and get the correct mean
  mad = sum(as.numeric(dist(incomes))) / (0.5 * n^2)
  rmad = mad / mean(incomes)
  gini = 0.5 * rmad * n/(n-1)
  
  # Get all possible pairs
  return(gini)
}

inc_men <- c(1,1,5)
inc_wom <- c(1,1,5)
inc_all <- c(inc_men, inc_wom)

calc_total_gini(inc_men)
calc_total_gini(inc_wom)
calc_total_gini(inc_all)




