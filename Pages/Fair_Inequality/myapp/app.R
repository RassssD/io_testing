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
      
      # Select of income type
      #selectInput(inputId = "distribution", label = h3("Income Distribution"), 
                  #choices = list("Fixed", "Random Normal", "Pareto", "Random Log Normal", "Siblings"), 
                  #selected = "Siblings"),
      
      
      #=========================================================================#
      # FIXED
      #=========================================================================#
      
      conditionalPanel(condition = "input.distribution == 'Fixed'",
        # Incomes
        p("Income distribution based on a median/mean income, a number of individuals in each group, and the difference in income between two consecutive individuals."),
        numericInput(inputId="fixed_n_in_group", label = h3("Number in each group"), value = 5),

        sliderInput("fixed_mean_income_slider", label = h3("Mean Income"), min = 0, 
                    max = 5, value = c(1, 2), step=0.25),
        sliderInput(inputId = "income_step_slider", label = h3("Income Step"), min = 0, 
                    max = 5, value = c(0.25, 0.5), step=0.25)

      ),
      
      #=========================================================================#
      # RANDOM NORMAL
      #=========================================================================#
      conditionalPanel(condition = "input.distribution == 'Random Normal'",
        numericInput(inputId="rn_n_in_group", label = h3("Number in each group"), value = 500),
        
        sliderInput("rn_mean_income_slider", label = h3("Mean Income"), min = 0, 
                    max = 100, value = c(25, 40), step=5),

        fluidRow(
          column(width = 6,
                 numericInput("rn_var_men", label=h5("Variance - Men"), value=10)
          ),
          column(width = 6, 
                 numericInput("rn_var_women", label=h5("Variance - Women"), value=5)
          )
        )
      ),
      
      
      #=========================================================================#
      # PARETO
      #=========================================================================#
      
      conditionalPanel(condition = "input.distribution == 'Pareto'",
       numericInput(inputId="pareto_n_in_group", label = h3("Number in each group"), value = 50),
       
       sliderInput("pareto_min_slider", label = h3("Mean Income"), min = 0, 
                   max = 100, value = c(25, 40), step=5),
       
       fluidRow(
         column(width = 6,
                numericInput("pareto_scale_men", label=h5("Scale - Men"), value=100)
         ),
         column(width = 6, 
                numericInput("pareto_scale_women", label=h5("Scale - Women"), value=50)
         )
       )
      ),
      

      #=========================================================================#
      # LOG NORMAL
      #=========================================================================#

      
      conditionalPanel(condition = "input.distribution == 'Random Log Normal'",
       numericInput(inputId="rln_n_in_group", label = h3("Number in each group"), value = 50),
       
       sliderInput("rln_mean_income_slider", label = h3("Mean Income"), min = 0, 
                   max = 100, value = c(20, 50), step=1),
       
       fluidRow(
         column(width = 6,
                numericInput("rln_var_men", label=h5("Variance - Men"), value=0.5)
         ),
         column(width = 6, 
                numericInput("rln_var_women", label=h5("Variance - Women"), value=1)
         )
       )
       #checkboxInput("checkbox", label = "Log Scale", value = FALSE),
      ), 
      
      #=========================================================================#
      # SIBLINGS
      #=========================================================================#
      
      conditionalPanel(condition = "1 == 1",#"input.distribution == 'Siblings'",
       numericInput(inputId="sib_n_in_group", label = h4("Number in each group"), value = 500),
       
       sliderInput("sib_phi", HTML("Unfair Inequality: <br/>Family Advantage, 1-φ"), min = 0, 
                   max = 1, value = 0.6, step=0.05),
       
       sliderInput("sib_mean_income_diff_slider", HTML("Unfair Inequality: Gender Gap (%)"), min = -100, 
                   max = 100, value = 25, step=5),
       sliderInput("sib_var_income_diff_slider", HTML("Within-gender Inequality <br/>% Difference in income variance"), min = -100, 
                   max = 100, value = -30, step=5)
       
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
    # FIXED
    #=========================================================================#
    
    if (distribution == "Fixed") {
      
      n_indivs = input$fixed_n_in_group
      
      base_income_woman <- input$fixed_mean_income_slider[1]
      base_income_man <- input$fixed_mean_income_slider[2]
      
      income_step_woman <- input$income_step_slider[1]
      income_step_man <- input$income_step_slider[2]
      n_indivs_step <- floor(n_indivs/2)
      
      incomes_woman <- to_vec(for(i in -n_indivs_step:n_indivs_step) max(base_income_woman + income_step_woman * i, 0))
      incomes_man <- to_vec(for(i in -n_indivs_step:n_indivs_step) max(base_income_man + income_step_man * i, 0))
      
      group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
      group_man <- to_vec(for(i in 1:n_indivs) "Man")
      
      # Logic for handling even numbers - remove the base number
      if (floor(n_indivs/2) == n_indivs/2) {
        incomes_woman <- incomes_woman[-(n_indivs/2+1)]
        incomes_man <- incomes_man[-(n_indivs/2+1)]
  
      }
    }
    
    #=========================================================================#
    # RANDOM NORMAL
    #=========================================================================#
    
    if (distribution == "Random Normal") {
      
      n_indivs = input$rn_n_in_group
      
      
      incomes_man <- pmax(rnorm(n_indivs, input$rn_mean_income_slider[2], input$rn_var_men), 0)
      incomes_woman <- pmax(rnorm(n_indivs, input$rn_mean_income_slider[1], input$rn_var_women), 0)
      
      group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
      group_man <- to_vec(for(i in 1:n_indivs) "Man")
      
    }
    
    #=========================================================================#
    # RANDOM LOG NORMAL
    #=========================================================================#
    
    if (distribution == "Random Log Normal") {
      
      n_indivs = input$rln_n_in_group
      
      
      incomes_man <- pmax(rlnorm(n_indivs, log(input$rln_mean_income_slider[2]), input$rln_var_men), 0)
      incomes_woman <- pmax(rlnorm(n_indivs, log(input$rln_mean_income_slider[1]), input$rln_var_women), 0)
      
      group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
      group_man <- to_vec(for(i in 1:n_indivs) "Man")
      
    }
    
    #=========================================================================#
    # PARETO
    #=========================================================================#
    
    if (distribution == "Pareto") {
      
      n_indivs = input$pareto_n_in_group
      
      incomes_man <- pmax(rpareto(n_indivs, input$pareto_min_slider[2], input$pareto_scale_men), 0)
      incomes_woman <- pmax(rpareto(n_indivs, input$pareto_min_slider[1], input$pareto_scale_women), 0)
      
      group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
      group_man <- to_vec(for(i in 1:n_indivs) "Man")
      
    }
    
    
    #=========================================================================#
    # SIBLINGS WITH RLN
    #=========================================================================#
    
    if (distribution == "Siblings") {
      
      n_indivs = input$sib_n_in_group
      sib_phi = input$sib_phi
      
      # Obtain parameters
      sib_RLN_mean_men = 10
      sib_RLN_mean_women = sib_RLN_mean_men - input$sib_mean_income_diff_slider * sib_RLN_mean_men / 100
      
      sib_RLN_var_men = 1
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
      incomes_man_sss = sib_phi * incomes_man_sss_random + (1-sib_phi) * incomes_man
      incomes_woman_sss = sib_phi * incomes_woman_sss_random + (1-sib_phi) * incomes_woman
      
      
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

    print(max(df$Income))
    
    hist_max_x <- ceiling(max(df$Income)/5)*5

    # Different bins depending on number of indivs
    # If fixed intervals, okay to just have individual levels / capped at 0.25
    if (distribution == "Fixed") {
      bins <- seq(from=0, to=hist_max_x, by=min(max(input$Delta, 0.25),1))
    }
    
    # RLN is annoying and weird, keep it separate for now
    else if (distribution == "Random Log Normal" | input$distribution == "Siblings") {
      
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
      print(bins)

    }
    
    
    
    freq = hist(df$Income, breaks=bins, include.lowest=TRUE, plot=FALSE)
    hist_max_y <- ceiling(max(freq$counts))
    #print(max(freq$counts))

    
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
      Gini_Table <- mutate(Gini_Table, "Total Inequality" = Total, "% Fair" = 100 * Within_SSS / Total) %>% select(-c(Within_SSS, Total))
      
    }
    
    else {
      Gini_Table <- calc_group_gini(df)
    }
    
    
    return(Gini_Table)
    
    
  }
)
  
}


shinyApp(ui = ui, server = server)


