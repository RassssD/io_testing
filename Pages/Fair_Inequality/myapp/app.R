library(shiny)
library(comprehenr)
library(tidyverse)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Select of income type
      selectInput(inputId = "distribution", label = h3("Income Distribution"), 
                  choices = list("Fixed", "Random Normal"), 
                  selected = "Fixed"),
      
      # Fixed Distribution
      conditionalPanel(condition = "input.distribution == 'Fixed'",
        # Incomes
        p("Income distribution based on a median/mean income, a number of individuals in each group, and the difference in income between two consecutive individuals."),
        numericInput(inputId="fixed_n_in_group", label = h3("Number in each group"), value = 5),

        sliderInput("fixed_mean_income_slider", label = h3("Mean Income"), min = 0, 
                    max = 5, value = c(1, 2), step=0.25),
        sliderInput(inputId = "income_step_slider", label = h3("Income Step"), min = 0, 
                    max = 5, value = c(0.25, 0.5), step=0.25)

      ),
      
      conditionalPanel(condition = "input.distribution == 'Random Normal'",
        numericInput(inputId="rn_n_in_group", label = h3("Number in each group"), value = 50),
        sliderInput("mean_income_slider", label = h3("Mean Income"), min = 0, 
                    max = 100, value = c(25, 40), step=5),
        numericInput(inputId="rn_var_men", label = h5("Variance - Men"), value = 3),
        numericInput(inputId="rn_var_women", label = h5("Variance - Women"), value = 5),
      ),


      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      plotOutput(outputId = "distPlot"),
      fluidRow(column(width=6, plotOutput(outputId = "GiniPlot")), column(width=3, tableOutput(outputId = "GiniTable")))

      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  incomes_df <- reactive({

    if (input$distribution == "Fixed") {
      
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
    
    if (input$distribution == "Random Normal") {
      
      n_indivs = input$rn_n_in_group
      
      incomes_man <- pmax(rnorm(n_indivs, input$mean_income_slider[2], input$rn_var_men), 0)
      incomes_woman <- pmax(rnorm(n_indivs, input$mean_income_slider[1], input$rn_var_women), 0)
      
      group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
      group_man <- to_vec(for(i in 1:n_indivs) "Man")
      
    }
    
    df <- data.frame(c(incomes_man, incomes_woman), c(group_man, group_woman))
    colnames(df) <- c("Income", "Group")

    return(df)
  })
  
  
  output$distPlot <- renderPlot({
    
    ##########
    df <- incomes_df()
    hist_max_x <- ceiling(max(df$Income)/5)*5
    hist_max_y <- ceiling(max(table(round(df$Income))) * 1.1)
    bins <- seq(from=0, to=hist_max_x, by=min(max(input$Delta, 0.25),1))

    ####
    p1 <- hist(subset(df, Group == "Woman")$Income, breaks=bins)
    p2 <- hist(subset(df, Group == "Man")$Income, breaks=bins)

    plot(p1, xlim=c(0,hist_max_x), ylim=c(0,hist_max_y), col=rgb(1,0,0,1/4), xlab="Income", ylab="Count", xaxs="i", yaxs="i", main="Income Distribution")
    plot(p2, xlim=c(0,hist_max_x), ylim=c(0,hist_max_y), col=rgb(0,0,1,1/4), add=T, xlab="Income", ylab="Count", xaxs="i", yaxs="i")
    legend(x = "topleft",         # Position
           legend = c("Women", "Men"), # Legend texts
           fill = c(2, 4))
    
    
  })
  
  output$GiniPlot <- renderPlot({
    
    ##########
    df <- incomes_df()
    
    n_indivs <- ifelse(input$distribution == "Fixed", input$fixed_n_in_group, input$rn_n_in_group)

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
  
  output$GiniTable <- renderTable({
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
    
    
    df <- incomes_df()
    return(calc_group_gini(df))
    
  })
  
}


shinyApp(ui = ui, server = server)


