
library(shiny)
library(comprehenr)
library(tidyverse)
library(EnvStats)
library(ggplot2)
library(cowplot)



#=========================================================================#
# DATA
#=========================================================================#

# Generate the data

gen_inc_df <- function(
    n_indivs, 
    phi_man, phi_woman, 
    mean_man, mean_woman, 
    var_man, var_woman) {
  
  # Default values
  if (phi_woman == -1) {
    phi_woman = phi_man
  }
  if (mean_woman == -1) {
    phi_woman = mean_man
  }
  if (var_woman == -1) {
    phi_woman = var_man
  }
  
  # Generate original incomes
  incomes_man <- pmax(rlnorm(n_indivs, log(mean_man), var_man), 0)
  incomes_woman <- pmax(rlnorm(n_indivs, log(mean_woman), var_woman), 0)
  
  group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
  group_man <- to_vec(for(i in 1:n_indivs) "Man")
  
  
  # Generate random element of the SSS income
  incomes_man_sss_random <- pmax(rlnorm(n_indivs, log(mean_man), var_man), 0)
  incomes_woman_sss_random <- pmax(rlnorm(n_indivs, log(mean_woman), var_woman), 0)
  
  # Combine the two to get the SSS income
  incomes_man_sss = phi_man * incomes_man_sss_random + (1-phi_man) * incomes_man
  incomes_woman_sss = phi_woman * incomes_woman_sss_random + (1-phi_woman) * incomes_woman
  
  # Generate DF
  df <- data.frame(c(incomes_man, incomes_woman), c(incomes_man_sss, incomes_woman_sss), c(group_man, group_woman))
  colnames(df) <- c("Income", "Income_SSS", "Group")
  
  return(df)
}



#=========================================================================#
# GINI CALC
#=========================================================================#

# Calculates gini for the entire population
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

# Sibling Gini within a population
calc_sib_gini <- function(dataframe) {
  
  ## Calculate Gini for entire population
  # Reshape such that there is only one income column
  dataframe_long <- dataframe %>%
    pivot_longer(cols = starts_with("Income"),
                 names_to = "Sibling",
                 values_to = "Income")
  
  # Within sibling
  n = length(dataframe_long$Income)
  
  mad = sum(abs(dataframe$Income - dataframe$Income_SSS)) / (0.5*n)
  rmad = mad / mean(dataframe_long$Income)
  gini = 0.5 * rmad * n/(n-1)
  
  return(gini)
  
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
  all_within_SSS_gini <- calc_sib_gini(dataframe)
  all_total_gini <- calc_total_gini(dataframe_long)
  
  # Fill in the All category
  df_gini[nrow(df_gini) + 1,] = list(Group = "All", Within_SSS = all_within_SSS_gini, Total = all_total_gini)
  
  # Iterate over groups
  for(group in unique(dataframe$Group)){
    # Calculate group Gini and sibling gini
    df_group <- dataframe %>% filter(Group == group)
    group_gini <- calc_total_gini(df_group)
    group_sib_gini <- calc_sib_gini(df_group)
    
    
    # Add to group
    df_gini[nrow(df_gini) + 1,] = list(Group = group, Within_SSS = group_sib_gini, Total = group_gini)
  }
  
  # Add % fair. If no inequality, say all is fair (since the world is "fair" anyway)
  df_gini <- df_gini %>% mutate(
    "ShareFair" = round(100 * Within_SSS / Total, 2))
  
  return(df_gini)
}


#=========================================================================#
# PLOTS
#=========================================================================#

# Function for plotting the Gini
plot_gini = function(df_data, x_title = "Param", y_title = "Gini") {
  
  step_param = 0.25
  
  x_min = min(df_data$ValParam)
  x_max = max(df_data$ValParam)
  
  y_min = 0 # floor(min(df_data$Total) / step_param) * step_param
  y_max = 1 # min(ceiling(max(df_data$Total) / step_param) * step_param, 1)
  
  
  # Plot
  df_data %>% group_by(ValParam, Group) %>% mutate(Mean_Gini = mean(Total)) %>%
    ggplot(aes(x=ValParam, y = Mean_Gini, color=Group)) + 
    #geom_line(size = 1) + 
    geom_smooth(size = 1, se=FALSE) + #, formula = y ~ x + x^2 + x^3) + 
    theme_classic() + 
    theme(title = element_text(size = 15), plot.title = element_text(hjust = 0.5), 
          legend.position = "none",
          panel.grid.major = element_line(color = "grey",
                                          size = 0.5,
                                          linetype = 2) ) +
    scale_color_manual(values=c("black", "blue", "red")) + 
    scale_x_continuous(limits=c(x_min, x_max), expand = c(0,0)) + scale_y_continuous(limits=c(y_min,y_max), expand = c(0,0)) + 
    xlab(x_title) + ylab(y_title) #+ 
    #ggtitle(sprintf("Total Inequality, Varying %s", param))
}


# Function for plotting the Share Fair
plot_SF = function(df_data, x_title = "Param", y_title = "% Fair") {
  
  step_param = 25
  
  x_min = min(df_data$ValParam)
  x_max = max(df_data$ValParam)
  
  y_min = 0 # floor(min(df_data$ShareFair) / step_param) * step_param
  y_max = 100 # min(ceiling(max(df_data$ShareFair) / step_param) * step_param, 100)
  
  # Plot
  df_data %>% group_by(ValParam, Group) %>% mutate(Mean_SF = mean(ShareFair)) %>%
    ggplot(aes(x=ValParam, y = Mean_SF, color=Group)) + 
    #geom_line(size = 1) + 
    geom_smooth(size = 1, se=FALSE) + #, formula = y ~ x + x^2 + x^3) + 
    theme_classic() + 
    theme(title = element_text(size = 15), plot.title = element_text(hjust = 0.5), 
          legend.position = "none",
          panel.grid.major = element_line(color = "grey",
                                                  size = 0.5,
                                                  linetype = 2) 
          
          
    ) +
    scale_color_manual(values=c("black", "blue", "red")) + 
    scale_x_continuous(limits=c(x_min, x_max), expand = c(0,0)) + 
    scale_y_continuous(limits=c(y_min,y_max), expand = c(0,0)) + 
    xlab(x_title) + ylab(y_title) #+ 
    #ggtitle(sprintf("Total Inequality, Varying %s", param))
}


# Get the plots, separate from the data generation
gen_plots = function(df_data, val_phi = 0.2, val_GG = -0.3, val_var = 1, n_select = 10) {
  
  plot_gini_phi = plot_gini(select_pregen_inc_data(df_preload_data, val_GG = val_GG, val_var = val_var, n_select = n_select), x_title = "", y_title = "Gini")
  plot_SF_phi = plot_SF(select_pregen_inc_data(df_preload_data, val_GG = val_GG, val_var = val_var, n_select = n_select), x_title = "Phi", y_title = "% Fair")
  
  # GG
  plot_gini_GG = plot_gini(select_pregen_inc_data(df_preload_data, val_phi = val_phi, val_var = val_var, n_select = n_select), x_title = "", y_title = "")
  plot_SF_GG = plot_SF(select_pregen_inc_data(df_preload_data, val_phi = val_phi, val_var = val_var, n_select = n_select), x_title = "GG", y_title = "")
  
  # Var
  plot_gini_var = plot_gini(select_pregen_inc_data(df_preload_data, val_phi = val_phi, val_GG = val_GG, n_select = n_select), "")
  plot_SF_var = plot_SF(select_pregen_inc_data(df_preload_data, val_phi = val_phi, val_GG = val_GG, n_select = n_select), x_title = "Var", y_title = "")
  
  # all together
  #plots = c(plot_gini_phi, plot_gini_GG, plot_gini_var, plot_SF_phi, plot_SF_GG, plot_SF_var)
  
  plot_grid = plot_grid(plot_gini_phi, plot_gini_GG, plot_gini_var, plot_SF_phi, plot_SF_GG, plot_SF_var, 
            ncol=3, nrow=2)
  
  return(plot_grid)
}




#=========================================================================#
# PRE-LOADING DATA
#=========================================================================#

#n_samples = 10

phi_min = 0
phi_max = 1
phi_step = 0.05

GG_min = -1
GG_max = 1
GG_step = 0.05

var_min = 0
var_max = 2
var_step = 0.05

mean_man = 10


calc_total_combs = function() {

  phi_list = seq(phi_min, phi_max, by=phi_step)
  GG_list = seq(GG_min, GG_max, by=GG_step)
  var_list = seq(var_min, var_max, by=var_step)
  
  # All combinations
  total_combs = length(phi_list) * length(GG_list) * length(var_list)
  
  return(total_combs)
}

calc_total_combs()

# Function to pregenerate the data
gen_preload_inc_data = function(n_samples = 10, n_indivs = 100) {
  
  # List of vars
  phi_list = seq(phi_min, phi_max, by=phi_step)
  GG_list = seq(GG_min, GG_max, by=GG_step)
  var_list = seq(var_min, var_max, by=var_step)
  
  # All combinations
  total_combs = length(phi_list) * length(GG_list) * length(var_list)
  
  df_param_combs = expand.grid(phi_list, GG_list, var_list)
  colnames(df_param_combs) = c("Phi", "GG", "Var")
  
  
  df_data = data.frame(matrix(ncol = 10, nrow = 0))
  colnames(df_data) <- c("Total_All", "Total_Man", "Total_Woman", "ShareFair_All", "ShareFair_Man", "ShareFair_Woman", "Phi", "GG", "Var", "N_Sim")
  
  # Loop over each combination of values
  for (i in 1:total_combs) {
    
    # Parameters for this iteration
    vals = df_param_combs[i, ]
    val_phi = vals$Phi
    val_GG = vals$GG
    val_var = vals$Var
    
    mean_woman = mean_man + val_GG * mean_man
    
    # Create N simulations for each combination
    for (n in 1:n_samples) {
      
      print(sprintf("Iteration %s / %s", i * n_samples + n, total_combs * n_samples))
      
      # Main data
      df <- gen_inc_df(n_indivs, phi_man = val_phi, phi_woman = val_phi, mean_man = mean_man, mean_woman = mean_woman, var_man = val_var, var_woman = val_var)
      
      # Calculate Gini etc
      df_gini = calc_sib_groub_gini(df) %>% select(c(Group, Total, ShareFair)) %>% pivot_wider(names_from = Group, values_from = c(Total, ShareFair))
      
      # Add param values
      df_gini$Phi = val_phi
      df_gini$GG = val_GG
      df_gini$Var = val_var
      df_gini$N_Sim = n
      
      # Append
      df_data <- df_data %>% bind_rows(df_gini)
      
      
    }
  }
  
  df_data_long = df_data %>%
    pivot_longer(cols = -c(Phi, GG, Var, N_Sim), 
                 names_to = c(".value", "Group"), 
                 names_pattern = "(Total|ShareFair)_(All|Man|Woman)")
  
  return(df_data_long)
}

df_preload_data = gen_preload_inc_data(n_samples = 10)

write.csv(df_preload_data, "C:/Programming/GitHub/Wendy-RA/Working/Pages/Fair_Inequality/myapp/pregen_data.csv")




# Function for selecting the needed data
# Specify the parameter to vary, 
select_pregen_inc_data = function(df_pregen_data, n_select = 10, param_to_vary = "X", val_phi = -1, val_GG = -1, val_var = -1) {
  
  ## Filter the appropriate each time, rename the desired column to fit drawing functions
  # Phi is the wanted parameter
  if (val_phi == -1) {
    df_pregen_data_filtered = df_pregen_data %>% filter(GG == val_GG, Var == val_var) %>% 
      mutate(ValParam = Phi) %>%
      select(-c(Phi, GG, Var))
  }
  
  # GG is the wanted parameter
  if (val_GG == -1) {
    df_pregen_data_filtered = df_pregen_data %>% filter(Phi == val_phi, Var == val_var) %>% 
      mutate(ValParam = GG) %>%
      select(-c(Phi, GG, Var))
  }
  
  # Var is the wanted parameter
  if (val_var == -1) {
    df_pregen_data_filtered = df_pregen_data %>% filter(GG == val_GG, Phi == val_phi) %>% 
      mutate(ValParam = Var) %>%
      select(-c(Phi, GG, Var))
  }
  
  
  # Draw a number of observations to create fake randomness
  # Need to figure this out, but it mostly works
  n_max = df_pregen_data_filtered %>% length()
  n_select = min(n_max, n_select)
  
  df_pregen_data_sampled = df_pregen_data_filtered[sample(nrow(df_pregen_data_filtered), n_select), ]
  
  return(df_pregen_data_filtered)
  
}

df_preload_data = read.csv("C:/Programming/GitHub/Wendy-RA/Working/Pages/Fair_Inequality/myapp/data/pregen_data.csv")

gen_plots(df_preload_data, val_phi = 0, val_GG = 0.9, val_var = 1)






#=========================================================================#
#=========================================================================#
# HOPEFULLY DEPRECATED
#=========================================================================#
#=========================================================================#


#=========================================================================#
# Varying phi (identically)
#=========================================================================#


# Function Version
gen_phi_sims = function(n_indivs = 100, n_samples = 100, list_phi = seq(0, 1, by=0.1), mean_man = 10, mean_woman = 10, var_man = 1, var_woman = 1) {

  # Storage
  df_all = data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_all) <- c("Total_All", "Total_Man", "Total_Woman", "ShareFair_All", "ShareFair_Man", "ShareFair_Woman", "ValParam")
  
  # Loop
  for (val_phi in list_phi) {
    
    # Create samples
    for (i in 1:n_samples) {
      # Create data and calculate Ginis
      df <- gen_inc_df(n_indivs, phi_man = val_phi, phi_woman = val_phi, mean_man = mean_man, mean_woman = mean_woman, var_man = var_man, var_woman = var_woman)
      df_gini = calc_sib_groub_gini(df) %>% select(c(Group, Total, ShareFair)) %>% pivot_wider(names_from = Group, values_from = c(Total, ShareFair))
      df_gini$ValParam = val_phi
      
      # Add data
      df_all <- df_all %>% bind_rows(df_gini)
    }
    
  }
  
  
  # Pivot Longer to get better data format
  df_all_long = df_all %>%
    pivot_longer(cols = -ValParam, 
                 names_to = c(".value", "Group"), 
                 names_pattern = "(Total|ShareFair)_(All|Man|Woman)")
  
  return(df_all_long)
}




df_test = gen_phi_sims(n_samples=50, var_man = 0.25, var_woman = 0.25)
graph_test = gen_plots(df_test, "Phi")

graph_test[[1]]

# Note: U-shaped relationship with phi? Only for the total, 2 others are basically linear
# Need quite a few samples to get sensible figures for the Gini coefficient, might be a good idea to pre-generate.

#=========================================================================#
# Varying Gender Gap
#=========================================================================#

# Function Version
gen_GG_sims = function(n_indivs = 100, n_samples = 10, list_GG = seq(-100, 100, by=10), mean_man = 10, var_man = 1, var_woman = 1, phi_man = 0.25, phi_woman = 0.25) {
  
  # Storage
  df_all = data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_all) <- c("Total_All", "Total_Man", "Total_Woman", "ShareFair_All", "ShareFair_Man", "ShareFair_Woman", "ValParam")
  
  # Loop
  for (val_GG in list_GG) {
    
    mean_woman = mean_man + val_GG * mean_man / 100
    
    # Create samples
    for (i in 1:n_samples) {
      # Create data and calculate Ginis
      df <- gen_inc_df(n_indivs, phi_man = phi_man, phi_woman = phi_woman, mean_man = mean_man, mean_woman = mean_woman, var_man = var_man, var_woman = var_woman)
      df_gini = calc_sib_groub_gini(df) %>% select(c(Group, Total, ShareFair)) %>% pivot_wider(names_from = Group, values_from = c(Total, ShareFair))
      df_gini$ValParam = val_GG
      
      # Add data
      df_all <- df_all %>% bind_rows(df_gini)
    }
    
  }
  
  
  # Pivot Longer to get better data format
  df_all_long = df_all %>%
    pivot_longer(cols = -ValParam, 
                 names_to = c(".value", "Group"), 
                 names_pattern = "(Total|ShareFair)_(All|Man|Woman)")
  
  return(df_all_long)
  
}


df_test = gen_GG_sims(n_samples=50, phi_man = 0.25, phi_woman = 0.5)
test = gen_plots(df_test, "Gender Gap (%)")

test[[1]]


#=========================================================================#
# Varying Variance (identically)
#=========================================================================#


# Function Version
gen_var_sims = function(n_indivs = 100, n_samples = 100, list_var = seq(0, 2, by=0.1), mean_man = 10, mean_woman = 10, phi_man = 0.25, phi_woman = 0.25) {
  
  # Storage
  df_all = data.frame(matrix(ncol = 7, nrow = 0))
  colnames(df_all) <- c("Total_All", "Total_Man", "Total_Woman", "ShareFair_All", "ShareFair_Man", "ShareFair_Woman", "ValParam")
  
  # Loop
  for (val_var in list_var) {
    
    # Create samples
    for (i in 1:n_samples) {
      # Create data and calculate Ginis
      df <- gen_inc_df(n_indivs, phi_man = phi_man, phi_woman = phi_woman, mean_man = mean_man, mean_woman = mean_woman, var_man = val_var, var_woman = val_var)
      df_gini = calc_sib_groub_gini(df) %>% select(c(Group, Total, ShareFair)) %>% pivot_wider(names_from = Group, values_from = c(Total, ShareFair))
      df_gini$ValParam = val_var
      
      # Add data
      df_all <- df_all %>% bind_rows(df_gini)
    }
    
  }
  
  
  # Pivot Longer to get better data format
  df_all_long = df_all %>%
    pivot_longer(cols = -ValParam, 
                 names_to = c(".value", "Group"), 
                 names_pattern = "(Total|ShareFair)_(All|Man|Woman)")
  
  return(df_all_long)
}

# Output
df_test = gen_var_sims(n_samples=100)
test = gen_plots(df_test, "Variance")

test[[2]]






