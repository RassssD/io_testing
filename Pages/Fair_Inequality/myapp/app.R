library(shiny)
library(comprehenr)
library(tidyverse)
library(EnvStats)
library(ggplot2)
#library(cowplot)
#library(imager)
library(gridExtra)


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
          panel.grid.major = element_line(color = "grey",size = 0.5,linetype = 2),
          plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")
          ) +
    scale_color_manual(values=c("black", "blue", "red")) +
    scale_x_continuous(limits=c(x_min, x_max), expand = c(0,0)) +
    scale_y_continuous(limits=c(y_min,y_max), expand = c(0,0)) +
    xlab(x_title) + ylab(y_title)
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
          panel.grid.major = element_line(color = "grey",size = 0.5,linetype = 2),
          plot.margin=unit(c(0.1,0.1,0.1,0.1),"cm")
    ) +
    scale_color_manual(values=c("black", "blue", "red")) +
    scale_x_continuous(limits=c(x_min, x_max), expand = c(0,0)) +
    scale_y_continuous(limits=c(y_min,y_max), expand = c(0,0)) +
    xlab(x_title) + ylab(y_title)
}



# Get the plots, separate from the data generation
gen_plots = function(df_data, val_phi = 0.2, val_GG = -0.3, val_var = 1, n_select = 10) {
  
  plot_gini_phi = plot_gini(select_pregen_inc_data(df_data, val_GG = val_GG, val_var = val_var, n_select = n_select), x_title = "", y_title = "Gini")
  plot_SF_phi = plot_SF(select_pregen_inc_data(df_data, val_GG = val_GG, val_var = val_var, n_select = n_select), x_title = "φ", y_title = "% Fair")
  
  # GG
  plot_gini_GG = plot_gini(select_pregen_inc_data(df_data, val_phi = val_phi, val_var = val_var, n_select = n_select), x_title = "", y_title = "")
  plot_SF_GG = plot_SF(select_pregen_inc_data(df_data, val_phi = val_phi, val_var = val_var, n_select = n_select), x_title = "GG", y_title = "")
  
  # Var
  plot_gini_var = plot_gini(select_pregen_inc_data(df_data, val_phi = val_phi, val_GG = val_GG, n_select = n_select), x_title = "", y_title = "")
  plot_SF_var = plot_SF(select_pregen_inc_data(df_data, val_phi = val_phi, val_GG = val_GG, n_select = n_select), x_title = "Var", y_title = "")
  
  
  # Direct grid
  plots = plot_grid(plot_gini_phi, plot_gini_GG, plot_gini_var, plot_SF_phi, plot_SF_GG, plot_SF_var, 
                    ncol=3, nrow=2)
  
  # Workaround
  list_plots = list(plot_gini_phi, plot_gini_GG, plot_gini_var, plot_SF_phi, plot_SF_GG, plot_SF_var)
  
  for (i in 1:6) {
    ggsave(filename = sprintf("./images/param_plots_%s.png", i), plot = list_plots[[i]],
           width = 4, height = 4)
  }
  
  
  
  rl = lapply(sprintf("./images/param_plots_%s.png", 1:6), png::readPNG)
  gl = lapply(rl, grid::rasterGrob)
  grid_plot = gridExtra::arrangeGrob(grobs=gl, nrow=2, ncol=3)
  
  ggsave(filename = "./images/param_plots_new.png", plot = grid_plot)
  
  return(plots)
}




#=========================================================================#
# PRE-LOADING DATA
#=========================================================================#


calc_total_combs = function() {
  
  phi_list = seq(phi_min, phi_max, by=phi_step)
  GG_list = seq(GG_min, GG_max, by=GG_step)
  var_list = seq(var_min, var_max, by=var_step)
  
  # All combinations
  total_combs = length(phi_list) * length(GG_list) * length(var_list)
  
  return(total_combs)
}


# Function to pregenerate the data
gen_preload_inc_data = function(n_samples = 10, n_indivs = 100) {
  
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

#df_preload_data = gen_preload_inc_data(n_samples = 10)
#write.csv(df_preload_data, "C:/Programming/GitHub/Wendy-RA/Working/Pages/Fair_Inequality/myapp/pregen_data.csv")


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
  #n_max = df_pregen_data_filtered %>% length()
  #n_select = min(n_max, n_select)
  
  #df_pregen_data_sampled = df_pregen_data_filtered[sample(nrow(df_pregen_data_filtered), n_select), ]
  
  return(df_pregen_data_filtered)
  
}




#=========================================================================#
# ACTUAL WEB CODE
#=========================================================================#

# Load the data
df_preload_data = read.csv("C:/Programming/GitHub/Wendy-RA/Working/Pages/Fair_Inequality/myapp/data/pregen_data.csv")

# Define UI for app that draws a histogram ----
ui <- tagList(
  
  # App title ----
  titlePanel("Analysing Fair Inequality"),
  
  navbarPage(
    "Page",
    tabPanel("Main",
      
      # Sidebar layout with input and output definitions ----
      sidebarLayout(
        
        #=========================================================================#
        # INPUT
        #=========================================================================#
        sidebarPanel(
          
          # SIBLINGS
          
          conditionalPanel(condition = TRUE,#"input.distribution == 'Siblings'",
                           numericInput(inputId="sib_n_in_group", label = h4("Number in each group"), value = 500),
                           
                           h4("Incomes are generated as follows:"),
                           
                           sliderInput("sib_phi_men", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ, men"), min = 0, 
                                       max = 1, value = 0.6, step=0.05),
                           sliderInput("sib_phi_women", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ, women"), min = 0, 
                                       max = 1, value = 0.6, step=0.05),
                           
                           sliderInput("sib_mean_income_diff_slider", HTML("Unfair Inequality: Gender Gap (%)"), min = -100, 
                                       max = 100, value = 25, step=5),
                           sliderInput("sib_var_income_slider", HTML("Variance (men)"), min = 0, 
                                       max = 5, value = 1, step=0.25),
                           sliderInput("sib_var_income_diff_slider", HTML("Within-gender Inequality <br/>% Difference in income variance"), min = -100, 
                                       max = 100, value = -25, step=25)
                           
          )
          
          , width = 4),
        
        #=========================================================================#
        # OUTPUT
        #=========================================================================#
        mainPanel(
          
          #plotOutput(outputId = "distPlot"),
          #fluidRow(column(width=6, plotOutput(outputId = "GiniPlot")), column(width=3, tableOutput(outputId = "GiniTable")))
          h3("Lorenz Curves"),
          plotOutput(outputId = "GiniPlot", width="455px", height="500px"),
          h3("Gini Coefficient"),
          tableOutput(outputId = "GiniTable")
          
          , width=8)
      )
    ),

    tabPanel("Secondary", 
             
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               
               #=========================================================================#
               # INPUT
               #=========================================================================#
               sidebarPanel(
                 
                 # SIBLINGS
                 
                 conditionalPanel(condition = TRUE,#"input.distribution == 'Siblings'",
                                  sliderInput("params_phi", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ, men"), min = 0, 
                                              max = 1, value = 0.6, step=0.05),
                                  sliderInput("params_GG", HTML("Unfair Inequality: Gender Gap (%)"), min = -1, 
                                              max = 1, value = -0.2, step=0.05),
                                  sliderInput("params_var_inc", HTML("Variance"), min = 0, 
                                              max = 2, value = 1, step=0.05),
                                  
                 )
                 
                 , width = 4),
               
               #=========================================================================#
               # OUTPUT
               #=========================================================================#
               mainPanel(
                 
                 #plotOutput(outputId = "param_plots", width="455px", height="500px"),
                 imageOutput("params_plot"),
                 "End"
                 
                 , width=8)
             )
             
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  incomes_df <- reactive({

    # For now, fix to siblings
    distribution = input$distribution
    distribution = "Siblings"
    
    #=========================================================================#
    # SIBLINGS WITH RLN
    #=========================================================================#
    
    if (distribution == "Siblings") {
      
      n_indivs = input$sib_n_in_group
      sib_phi_men = input$sib_phi_men
      sib_phi_women = input$sib_phi_women
      
      
      # Obtain parameters
      sib_RLN_mean_men = 10
      sib_RLN_mean_women = sib_RLN_mean_men - input$sib_mean_income_diff_slider * sib_RLN_mean_men / 100
      
      sib_RLN_var_men = input$sib_var_income_slider
      sib_RLN_var_women = sib_RLN_var_men + input$sib_var_income_diff_slider * sib_RLN_var_men / 100
      
      # Generate original incomes
      incomes_man <- pmax(rlnorm(n_indivs, log(sib_RLN_mean_men), sib_RLN_var_men), 0)
      incomes_woman <- pmax(rlnorm(n_indivs, log(sib_RLN_mean_women), sib_RLN_var_women), 0)
      
      group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
      group_man <- to_vec(for(i in 1:n_indivs) "Man")
      

      # Generate random element of the SSS income
      incomes_man_sss_random <- pmax(rlnorm(n_indivs, log(sib_RLN_mean_men), sib_RLN_var_men), 0)
      incomes_woman_sss_random <- pmax(rlnorm(n_indivs, log(sib_RLN_mean_women), sib_RLN_var_women), 0)
      
      # Combine the two to get the SSS income
      incomes_man_sss = sib_phi_men * incomes_man_sss_random + (1-sib_phi_men) * incomes_man
      incomes_woman_sss = sib_phi_women * incomes_woman_sss_random + (1-sib_phi_women) * incomes_woman
      
      
    }
    
    # If siblings, need to have some extra columns to make it work
    if (distribution == "Siblings") {
      df <- data.frame(c(incomes_man, incomes_woman), c(incomes_man_sss, incomes_woman_sss), c(group_man, group_woman))
      colnames(df) <- c("Income", "Income_SSS", "Group")
    }
    
    # Otherwise don't bother
    else {
      df <- data.frame(c(incomes_man, incomes_woman), c(group_man, group_woman))
      colnames(df) <- c("Income", "Group")
    }

    return(df)
  })
  
  
  #=========================================================================#
  # OUTPUT PLOTS
  #=========================================================================#
  
  
  # Distribution Plot
  output$distPlot <- renderPlot({
    
    ##########
    df <- incomes_df()
    
    # For now, fix to siblings
    distribution = input$distribution
    distribution = "Siblings"
    
    if (distribution == "Siblings") {
      df <- df %>%
        pivot_longer(cols = starts_with("Income"),
                     names_to = "Sibling",
                     values_to = "Income")
    }
    
    hist_max_x <- ceiling(max(df$Income)/5)*5


    
    max_income_men = ceiling(max(subset(df, Group == "Man")$Income)/5)*5
    max_income_women = ceiling(max(subset(df, Group == "Woman")$Income)/5)*5
    

    
    rln_hist_break = 10^(floor(log10(min(max_income_men, max_income_women))))
    bins <- seq(from=0, to=hist_max_x+5*rln_hist_break, by=rln_hist_break)

    
    freq = hist(df$Income, breaks=bins, include.lowest=TRUE, plot=FALSE)
    hist_max_y <- ceiling(max(freq$counts))
    #print(max(freq$counts))


    p1 <- hist(subset(df, Group == "Woman")$Income, breaks=bins)
    p2 <- hist(subset(df, Group == "Man")$Income, breaks=bins)
    
    plot(p1, xlim=c(0,hist_max_x), ylim=c(0,hist_max_y), col=rgb(1,0,0,1/4), xlab="Income", ylab="Count", xaxs="i", yaxs="i", main="Income Distribution")
    plot(p2, xlim=c(0,hist_max_x), ylim=c(0,hist_max_y), col=rgb(0,0,1,1/4), add=T, xlab="Income", ylab="Count", xaxs="i", yaxs="i")
    legend(x = "topright",         # Position
           legend = c("Women", "Men"), # Legend texts
           fill = c(2, 4))

    
    
  })
  
  
  # Lorenz Curves
  output$GiniPlot <- renderPlot({
    
    df <- incomes_df()
    
    n_indivs <- nrow(df)/ 2

    #n_indivs <- ifelse(input$distribution == "Fixed", input$fixed_n_in_group, input$rn_n_in_group)

    df <- df[order(df$Group, df$Income),]

    df <- df %>% group_by(Group) %>% mutate(Cum_Income_Share = cumsum(Income) / sum(Income)) %>% ungroup()
    

    df_draw_men <- df %>% filter(Group == "Man")
    cum_inc_share_men <- c(0) %>% append(df_draw_men$Cum_Income_Share)
    
    df_draw_women <- df %>% filter(Group == "Woman")
    cum_inc_share_women <- c(0) %>% append(df_draw_women$Cum_Income_Share)
    
    df <- df[order(df$Income),]
    df <- df %>% mutate(Cum_Income_Share_All = cumsum(Income) / sum(Income))
    cum_inc_share_all <- c(0) %>% append(df$Cum_Income_Share_All)
    
    steps = seq(0, 1, by=1/n_indivs)
    steps_all = seq(0, 1, by=0.5/n_indivs)


    plot(x=steps, y=steps, 
         type="l", col=1, lty=2, lwd=2,
         xlab="Cumulative Population", ylab="Cumulative Income", main="Lorenz Curves",
         xlim = c(0,1), ylim = c(0,1), xaxs="i", yaxs="i",
         asp=1)

    lines(x=steps, y=cum_inc_share_women, col=2, type="l", lwd=2)
    lines(x=steps, y=cum_inc_share_men, col=4, type="l", lwd=2)
    lines(x=steps_all, y=cum_inc_share_all, col=1, type="l", lwd=2)
    legend(x = "topleft",         # Position
           legend = c("Equality", "Women", "Men", "All"), # Legend texts
           lty = c(2, 1, 1, 1),
           col = c(1, 2, 4, 1),
           lwd = 2)

    
  })
  
  # Table
  output$GiniTable <- renderTable({
    
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
        df_group <- df %>% filter(Group == group)
        
        group_gini <- calc_total_gini(df_group)
        group_sib_gini <- calc_sib_gini(df_group)
        
        
        # Add to group
        df_gini[nrow(df_gini) + 1,] = list(Group = group, Within_SSS = group_sib_gini, Total = group_gini)
      }
      
      return(df_gini)
    }
    
    
    
    
    df <- incomes_df()
    
    # For now, fix to siblings
    distribution <- input$distribution
    distribution <- "Siblings"
    
    if (distribution == "Siblings") {
      Gini_Table <- calc_sib_groub_gini(df)
      Gini_Table <- mutate(Gini_Table, "Total Inequality" = Total, "% Fair" = 100 * Within_SSS / Total)# %>% select(-c(Within_SSS, Total))
      
    }
    
    else {
      Gini_Table <- calc_group_gini(df)
    }
    
    
    return(Gini_Table)
    
    
  })
  
  
  # Plot grid
  param_plot_grid = reactive({

    list_plots = gen_plots(df_preload_data, 
                      val_phi = input$params_phi, 
                      val_GG = input$params_GG, 
                      val_var = input$params_var_inc)
    
  })
  
  output$params_plot = renderImage({
    
    
    
    #rl = lapply(sprintf("my_viz%i.png", 1:4), png::readPNG)
    #gl = lapply(rl, grid::rasterGrob)
    #gridExtra::grid.arrange(grobs=gl)
    
    
    #save_plot(filename = "./images/param_plots.png", plot = param_plot_grid())
    list(src = "./images/param_plots_new.png",
         width=600,
         height=400)
  }, deleteFile = FALSE)
  
}



shinyApp(ui = ui, server = server)


