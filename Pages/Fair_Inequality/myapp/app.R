library(shiny)
library(comprehenr)
library(tidyverse)
library(EnvStats)
library(ggplot2)
library(cowplot)


create_app = function() {
  setwd("C:/Programming/GitHub/io_testing/Pages/Fair_Inequality")
  shinylive::export(appdir = "myapp", destdir = "docs")
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


# Generate cowplot grid
gen_plots_cowplot = function(df_data, val_phi = 0.2, val_GG = -0.3, val_var = 1, n_select = 10) {
  
  plot_gini_phi = plot_gini(select_pregen_inc_data(df_data, val_GG = val_GG, val_var = val_var, n_select = n_select), x_title = "", y_title = "Gini")
  plot_SF_phi = plot_SF(select_pregen_inc_data(df_data, val_GG = val_GG, val_var = val_var, n_select = n_select), x_title = "Phi", y_title = "% Fair")
  
  # GG
  plot_gini_GG = plot_gini(select_pregen_inc_data(df_data, val_phi = val_phi, val_var = val_var, n_select = n_select), x_title = "", y_title = "")
  plot_SF_GG = plot_SF(select_pregen_inc_data(df_data, val_phi = val_phi, val_var = val_var, n_select = n_select), x_title = "GG", y_title = "")
  
  # Var
  plot_gini_var = plot_gini(select_pregen_inc_data(df_data, val_phi = val_phi, val_GG = val_GG, n_select = n_select), x_title = "", y_title = "")
  plot_SF_var = plot_SF(select_pregen_inc_data(df_data, val_phi = val_phi, val_GG = val_GG, n_select = n_select), x_title = "Var", y_title = "")
  
  # all together
  #plots = c(plot_gini_phi, plot_gini_GG, plot_gini_var, plot_SF_phi, plot_SF_GG, plot_SF_var)
  
  plots = plot_grid(plot_gini_phi, plot_gini_GG, plot_gini_var, plot_SF_phi, plot_SF_GG, plot_SF_var, 
                    ncol=3, nrow=2)
  
  return(plots)
}



#=========================================================================#
# DATA
#=========================================================================#

# Load the data
df_preload_data = read.csv("./data/pregen_data.csv")


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


df_selected = select_pregen_inc_data(df_preload_data, n_select = 10, param_to_vary = "Phi", val_GG = 0.2, val_var = 1)


#=========================================================================#
#=========================================================================#
# ACTUAL WEB CODE
#=========================================================================#
#=========================================================================#

# UI
ui <- tagList(
  
  # App title ----
  titlePanel("Analysing Fair Inequality"),
  withMathJax(),
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
                 
                 conditionalPanel(condition = "'1' == '1'",#"input.distribution == 'Siblings'",
                                  numericInput(inputId="sib_n_in_group", label = h4("Number in each group"), value = 500),
                                  
                                  sliderInput("sib_phi_men", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ"), min = 0, 
                                              max = 1, value = 0.6, step=0.05),
                                  # sliderInput("sib_phi_women", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ, women"), min = 0, 
                                  #             max = 1, value = 0.6, step=0.05),
                                  
                                  sliderInput("sib_mean_income_diff_slider", HTML("Unfair Inequality: Gender Gap (%)"), min = -100, 
                                              max = 100, value = -25, step=5),
                                  sliderInput("sib_var_income_slider", HTML("Variance"), min = 0, 
                                              max = 5, value = 1, step=0.25),
                                  # sliderInput("sib_var_income_diff_slider", HTML("Within-gender Inequality <br/>% Difference in income variance"), min = -100, 
                                  #             max = 100, value = -25, step=25)
                                  
                 )
                 
                 , width = 4),
               
               #=========================================================================#
               # OUTPUT
               #=========================================================================#
               mainPanel(
                 
                 #plotOutput(outputId = "distPlot"),
                 #fluidRow(column(width=6, plotOutput(outputId = "GiniPlot")), column(width=3, tableOutput(outputId = "GiniTable")))
                 
                 h3("Data Generation"),
                 helpText('For each gender \\( g \\in \\{M,F\\} \\), and each sibling pair \\( i=1, \\dots, N \\), take two log-normally distributed variables, \\( X_{gi}^{(1)}, X_{gi}^{(2)} \\), with mean \\( \\mu_{g} \\) and variance \\( \\sigma_{g}^{2} \\). For gender \\(g\\), the family advantage is \\(1-\\varphi_{g}.\\)'),
                 helpText("Let \\( y_{gi}^{(1)}, y_{gi}^{(2)} \\) denote the log incomes of siblings 1 and 2 of a given pair \\( i \\) and a given gender \\( g \\)."),
                 helpText('Then \\( y_{gi}^{(1)} = X_{gi}^{(1)} \\) and \\( y_{gi}^{(2)} = \\varphi_{g} \\times X_{gi}^{(2)} + (1 - \\varphi_{g}) \\times X_{gi}^{(1)} \\).
'),
                 
                 
                 
                 h3("Lorenz Curves"),
                 plotOutput(outputId = "GiniPlot", width="455px", height="500px"),
                 
                 
                 h3("Gini Coefficient"),
                 
                 helpText('For each group \\( k \\in \\{p,f\\} \\) the Gini coefficient is: \\( G^{k}= \\displaystyle\\frac{\\Delta^{k}}{N^{k}} \\frac{1}{\\underline{y}} \\frac{1}{2} \\).'),
                 helpText('Where \\( \\Delta^{k} = \\sum_{i=j+1}^{n} \\sum_{j=1}^{n-1} |y_{i}-y_{j}| s_{ij}^{k} \\), such that \\( s_{ij}^{k}=1 \\) iff \\( i \\) and \\( j \\) are in the same group \\( k \\), and 0 otherwise. Also, \\( N^{p} = \\frac{n(n-1)}{2}, N^{f} = \\frac{n}{2} \\).'),
                 helpText('The share of fair inequality is defined as \\( \\cfrac{G^{f}}{G^{p}} \\).'),
                 
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
                 
                 conditionalPanel(condition = "'1' == '1'",#"input.distribution == 'Siblings'",
                                  sliderInput("params_phi", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ"), min = 0, 
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
                 withMathJax(),
                 plotOutput("param_plot", width="600px", height="400px")
                 
                 , width=8)
             )
             
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  #=========================================================================#
  # REACTIVE HELPER FUNCTIONS
  #=========================================================================#
  
  # generate random incomes df
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
      sib_phi_women = input$sib_phi_men
      
      
      # Obtain parameters
      sib_RLN_mean_men = 10
      sib_RLN_mean_women = sib_RLN_mean_men + input$sib_mean_income_diff_slider * sib_RLN_mean_men / 100
      
      sib_RLN_var_men = input$sib_var_income_slider
      sib_RLN_var_women = input$sib_var_income_slider
  
      
      #print(c(n_indivs, sib_phi_men, sib_phi_women, sib_RLN_mean_men, sib_RLN_mean_women, sib_RLN_var_men, sib_RLN_var_women))
          
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
  
  
  # Testing
  selected_df = reactive({

    test_string = sprintf("Test %s, %s, %s", input$params_phi, input$params_GG, input$params_var_inc)
    
    return(test_string)
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


    
    # RLN is annoying and weird, keep it separate for now
    if (distribution == "Random Log Normal" | input$distribution == "Siblings") {
      
      # FOR NOW, JUST DON'T BOTHER WITH THIS
      
      max_income_men = ceiling(max(subset(df, Group == "Man")$Income)/5)*5
      max_income_women = ceiling(max(subset(df, Group == "Woman")$Income)/5)*5
      
#      hist_max_x = min(max_income_men, max_income_women)
      
      rln_hist_break = 10^(floor(log10(min(max_income_men, max_income_women))))
      bins <- seq(from=0, to=hist_max_x+5*rln_hist_break, by=rln_hist_break)
      
      

    }
    
    # For RN, makes more sense to group more
    else {
      bins <- seq(from=0, to=hist_max_x, by=1)

    }
    
    
    
    freq = hist(df$Income, breaks=bins, include.lowest=TRUE, plot=FALSE)
    hist_max_y <- ceiling(max(freq$counts))

    
    if (distribution == "Random Log Normal") {
      p1 <- hist(subset(df, Group == "Woman")$Income, breaks = 20)#, breaks=bins)
      p2 <- hist(subset(df, Group == "Man")$Income, breaks=20)#, breaks=bins)
      
      plot(p1, ylim=c(0,hist_max_y), col=rgb(1,0,0,1/4), xlab="Income", ylab="Count", xaxs="i", yaxs="i", main="Income Distribution")
      plot(p2, ylim=c(0,hist_max_y), col=rgb(0,0,1,1/4), add=T, xlab="Income", ylab="Count", xaxs="i", yaxs="i")
      legend(x = "topright",         # Position
             legend = c("Women", "Men"), # Legend texts
             fill = c(2, 4))
    }


    else {
      p1 <- hist(subset(df, Group == "Woman")$Income, breaks=bins)
      p2 <- hist(subset(df, Group == "Man")$Income, breaks=bins)
      
      plot(p1, xlim=c(0,hist_max_x), ylim=c(0,hist_max_y), col=rgb(1,0,0,1/4), xlab="Income", ylab="Count", xaxs="i", yaxs="i", main="Income Distribution")
      plot(p2, xlim=c(0,hist_max_x), ylim=c(0,hist_max_y), col=rgb(0,0,1,1/4), add=T, xlab="Income", ylab="Count", xaxs="i", yaxs="i")
      legend(x = "topright",         # Position
             legend = c("Women", "Men"), # Legend texts
             fill = c(2, 4))
      
    }

    
    
  })
  
  
  # Lorenz Curves
  output$GiniPlot <- renderPlot({
    
    ##########
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
      #print(dataframe_long)
      # Within sibling
      n = length(dataframe_long$Income)
      n_pairs = length(dataframe$Income)
      #print(n)
      #print(n_pairs)
      
      mad = sum(abs(dataframe$Income - dataframe$Income_SSS)) / (n_pairs)
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
        df_long_group <- dataframe_long %>% filter(Group == group)
        
        group_gini <- calc_total_gini(df_long_group)
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
      Gini_Table <- mutate(Gini_Table, "Total Inequality" = Total, "% Fair" = min(100 * Within_SSS / Total, 100)) %>% select(-c(Total, Within_SSS))
      #print(df)
    }
    
    else {
      Gini_Table <- calc_group_gini(df)
    }
    
    
    return(Gini_Table)
    
    
  })
  
  
  
  # Secondary tab
  
  # For testing
  output$text_test = renderText(selected_df())
  
  
  # Parameter plot
  
  output$param_plot = renderPlot({
    
    gen_plots_cowplot(df_preload_data, input$params_phi, input$params_GG, input$params_var_inc)
    
  })
  
  
  # Texts
  
  # Income generation
  # output$inc_gen = renderUI({
  #   withMathJax(
  # 
  # })
  
  
  
}


shinyApp(ui = ui, server = server)


