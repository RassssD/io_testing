library(shiny)
library(comprehenr)
library(tidyverse)
library(EnvStats)
library(ggplot2)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Analysing Fair Inequality"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      #=========================================================================#
      # SIBLINGS
      #=========================================================================#
      
      conditionalPanel(condition = "'1' == '1'",#"input.distribution == 'Siblings'",
                       numericInput(inputId="sib_n_in_group", label = h4("Number in each group"), value = 500),
                       
                       sliderInput("sib_phi", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ"), min = 0, 
                                   max = 1, value = 0.6, step=0.05),
                       
                       sliderInput("sib_mean_income_diff_slider", HTML("Unfair Inequality: Gender Gap (%)"), min = 0, 
                                   max = 100, value = 25, step=5),
                       #sliderInput("sib_var_income_diff_slider", HTML("Within-gender Inequality <br/>% Difference in income variance"), min = -100, max = 100, value = -30, step=5),
                       fluidRow(
                         column(width = 6,
                                numericInput("sib_rln_var_men_test", label=h5("Variance - Men"), value=1)
                         ),
                         column(width = 6, 
                                numericInput("sib_rln_var_women_test", label=h5("Variance - Women"), value=1)
                         )
                       )
                       
      )
      
      , width = 4),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      #plotOutput(outputId = "distPlot"),
      #fluidRow(column(width=6, plotOutput(outputId = "GiniPlot")), column(width=3, tableOutput(outputId = "GiniTable")))
      plotOutput(outputId = "GiniPlot", width="455px", height="500px"),
      tags$h3("Gini Coefficient"),
      tableOutput(outputId = "GiniTable")
      
      
      
      , width=8)
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
    
    # Setup
    df <- df %>%
      pivot_longer(cols = starts_with("Income"),
                   names_to = "Sibling",
                   values_to = "Income")
    
    hist_max_x <- ceiling(max(df$Income)/5)*5
    

    max_income_men = ceiling(max(subset(df, Group == "Man")$Income)/5)*5
    max_income_women = ceiling(max(subset(df, Group == "Woman")$Income)/5)*5
    
    
    rln_hist_break = 10^(floor(log10(min(max_income_men, max_income_women))))
    bins <- seq(from=0, to=hist_max_x+5*rln_hist_break, by=rln_hist_break)
    
    freq = hist(df$Income, breaks=bins, include.lowest=TRUE, plot=FALSE)
    hist_max_y <- ceiling(max(freq$counts))
    

    # Plotting
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
      Gini_Table <- mutate(Gini_Table, "Total Inequality" = Total, "% Fair" = 100 * Within_SSS / Total) %>% select(-c(Total, Within_SSS))
      #print(df)
    }
    
    else {
      Gini_Table <- calc_group_gini(df)
    }
    
    
    return(Gini_Table)
    
    
  }
  )
  
}


shinyApp(ui = ui, server = server)