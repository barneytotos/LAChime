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
      h3("Data", style = "color:blue"),
      fluidRow(
        column(
          4,
          numericInput(
            "N", 
            "Regional population", 
            value = 4119405
          )
        ),
        column(
          4,
          numericInput(
            inputId = "market_share",
            label = "Hospital market share",
            min = 0,
            max = 1,
            value = 0.15
          )
        ),
        column(
          4,
          numericInput(
            "I", 
            "Currently hospitalized", 
            value = 14
          )
        )
      ),
      #################################
      ## Unknown Parameters
      ##################################
      h3("SIR parameters", style = "color:blue"),
      fluidRow(
        column(
          6,
          br(),
          br(),
          h4(strong("Doubling time", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "doubling_time_mean", 
            h5("Mean"), 
            value = 4,
            min = 1,
            max =100
          )
        ),
        column(
          3,
          numericInput(
            "doubling_time_sd", 
            h5("SD"), 
            value = 0.001,
            min = 0.00001,
            max =100
          )
        )
      ),
      fluidRow(
        column(
          6,
          br(),
          h4(strong("Contagious time", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "recovery_time_mean", 
            "", #h5("Mean"), 
            value = 14,
            min = 1,
            max =100
          )
        ),
        column(
          3,
          numericInput(
            "recovery_time_sd", 
            "", #h5("SD"), 
            value = 0.001,
            min = 0.00001,
            max =100
          )
        )
      ),
      fluidRow(
        column(
          6,
          br(),
          br(),
          h4(strong("Hospitalization rate", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "hospitalization_mean", 
            h5("Mean"), 
            value = 0.025,
            min = 0.001,
            max = 1
          )
        ),
        column(
          3,
          numericInput(
            "hospitalization_weight", 
            h5("Weight"), 
            value = 1000,
            min = 1,
            max =1000000
          )
        )
      ),
      fluidRow(
        column(
          6,
          br(),
          h4(strong("Contact reduction", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "contact_reduction_mean", 
            "", #h5("Mean"), 
            value = 0.3,
            min = 0.001,
            max = 1
          )
        ),
        column(
          3,
          numericInput(
            "contact_reduction_weight", 
            "", #h5("Weight"), 
            value = 1000,
            min = 1,
            max =1000000
          )
        )
      ),
      #################################
      ## Demand parameters Parameters
      ##################################
      h3("Demand parameters", style = "color:blue"),
      fluidRow(
        column(
          6,
          br(),
          br(),
          h4(strong("Hospitalization", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "hospitalization_glarg", 
            h5("Proportion"), 
            value = 1,
            min = 1,
            max = 1
          )
        ),
        column(
          3,
          numericInput(
            "hospitalization_los", 
            h5("LOS"), 
            value = 7,
            min = 1,
            max =100
          )
        )
      ),
      fluidRow(
        column(
          6,
          br(),
          h4(strong("ICU", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "icu_relative_rate", 
            "", #h5("Mean"), 
            value = 0.3,
            min = 0.001,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "icu_los", 
            "", #h5("Weight"), 
            value = 9,
            min = 1,
            max =100
          )
        )
      ),
      fluidRow(
        column(
          6,
          br(),
          h4(strong("Ventilator", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "ventilator_relative_rate", 
            "", #h5("Mean"), 
            value = 0.2,
            min = 0.001,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "ventilator_los", 
            "", #h5("Weight"), 
            value = 10,
            min = 1,
            max =100
          )
        )
      ),
      #################################
      ## Simulation parameters
      ##################################
      h3("Simulation settings", style = "color:blue"),
      fluidRow(
        column(
          3,
          numericInput(
            "future_days", 
            "N. Days ", 
            value = 50,
            min = 2
          )
        ),
        column(
          3,
          numericInput(
            "scaler", 
            "Scale Y", 
            value = 1,
            min = 0.0001
          )
        ),
        column(
          3,
          numericInput(
            "sims", 
            "Simulations", 
            value = 10**3,
            min = 2
          )
        ),
        column(
          3,
          br(),
          actionButton("runButton", "Simulate")
        )
      )
      
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Hospital forecast",        imageOutput("hospital")),
        tabPanel("ICU forecast",             imageOutput("icu")),
        tabPanel("Ventilator forecast",      imageOutput("ventilator"))
        # tabPanel("Census forecast", imageOutput("census"))
      )
    )
  )
)