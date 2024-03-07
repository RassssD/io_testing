library(shiny)
library(comprehenr)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Hello Shiny!"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "Delta",
                  label = "Delta:",
                  min = 0,
                  max = 5,
                  value = 0.5,
                  step=0.25),
      sliderInput(inputId = "n_in_group",
                  label = "Number in each group",
                  min = 1,
                  max = 10,
                  value = 5,
                  step = 2),
      sliderInput(inputId = "mu_man",
                  label = "Average Man Income",
                  min = 1,
                  max = 10,
                  value = 5,
                  step = 1),
      sliderInput(inputId = "mu_woman",
                  label = "Average Woman Income",
                  min = 1,
                  max = 10,
                  value = 2,
                  step = 1),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    ##########
    n_indivs = input$n_in_group
    
    base_income_woman = 1
    base_income_man = 2*base_income_woman
    
    income_step = input$Delta
    n_indivs_step = floor(n_indivs/2)
    print(n_indivs_step)
    
    incomes_woman = to_vec(for(i in -n_indivs_step:n_indivs_step) max(base_income_woman + income_step * i, 0))
    incomes_man = to_vec(for(i in -n_indivs_step:n_indivs_step) max(base_income_man + 2*income_step * i, 0))
    
    incomes_woman = rnorm(n_indivs, input$mu_woman, 1)
    incomes_man = rnorm(n_indivs, input$mu_man, 1)
    
    group_woman <- to_vec(for(i in 1:n_indivs) "Woman")
    group_man <- to_vec(for(i in 1:n_indivs) "Man")
    

    
    df = data.frame(c(incomes_man, incomes_woman), c(group_man, group_woman))
    colnames(df) <- c("Income", "Group")
    
    
    ####
    
    p1 <- hist(incomes_woman)
    p2 <- hist(incomes_man)
    
    plot(p1, xlim=c(0,10), col=rgb(1,0,0,1/4))
    plot(p2, xlim=c(0,10), col=rgb(0,0,1,1/4), add=T)

    
  })
  
  
}


shinyApp(ui = ui, server = server)


