library(shiny)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      # Input: Slider for the number of bins ----
      
      #################################
      ## POPULATION LEVEL PARAMETER
      ##################################
      titlePanel("Population parameters"),
      numericInput(
        "N", 
        "Regional population", 
        value = 4119405
      ), 
      numericInput(
        "I", 
        "Currently hospitalized COVID-19 patients", 
        value = 14
      ), 
      numericInput(
        inputId = "market_share",
        label = "Hospital market share\n(proportion of total population)",
        min = 0,
        max = 1,
        value = 0.15
      ),
      numericInput(
        inputId = "rate_hospitalized",
        label = "Proportion hospitalized (of infected population)",
        min = 0,
        max = 1,
        value = 0.025
      ),
      #################################
      ## SIR Parameters
      ##################################
      br(),
      titlePanel("SIR parameters"),
      numericInput(
        "doubling_time", 
        "Doubling time", 
        value = 4
      ), 
      numericInput(
        "recovery_time", 
        "Recovery time", 
        value = 14
      ), 
      sliderInput(
        inputId = "contact_reduction",
        label = "Proportion reduction in social contact (social distancing)",
        min = 0,
        max = 1,
        value = 0.30
      ),
      #################################
      ## Demand parameters Parameters
      ##################################
      br(),
      titlePanel("Demand parameters"),
      # Input: Slider for the number of bins ----
      numericInput(
        "length_of_stay", 
        h3("Hospital length of stay"), 
        value = 7
      ),
      # Input: Slider for the number of bins ----
      numericInput(
        "future_days", 
        h3("Number of days to project"), 
        value = 50
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Admissions forecast", imageOutput("demand")),
        tabPanel("Census forecast", imageOutput("census"))
      )
    )
  )
)