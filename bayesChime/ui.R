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
      h3("Assumption", style = "color:blue"),
      numericInput(
        "p", 
        "Proportion of newly infected people that test positive", 
        value = 0.1,
        min = 0.0001,
        max = 1
      ),
      numericInput(
        "lag", 
        "Lag between initial infection and confirmed infection", 
        value = 13,
        min = 0.0,
        max = 30
      ),
      numericInput(
        "social", 
        "Relative reduction in transmission (social distancing)", 
        value = 1,
        min = 0.001,
        max = 1
      ),
      #################################      ## Demand parameters Parameters
      ##################################
      h3("Demand parameters", style = "color:blue"),
      fluidRow(
        column(
          6,
          br(),
          br(),
          h4(strong("Hospitalized", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "hospital_relative_rate", 
            h5("Proportion"), 
            value = 0.1,
            min = 0.001,
            max = 1
          )
        ),
        column(
          3,
          numericInput(
            "hospital_los", 
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
          h4(strong("ICU", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "icu_relative_rate", 
            NULL, 
            value = 0.03,
            min = 0.001,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "icu_los", 
            NULL, 
            value = 9,
            min = 1,
            max =100
          )
        )
      ),
      fluidRow(
        column(
          6,
          h4(strong("Ventilator", align="bottom"))
        ),
        column(
          3,
          numericInput(
            "ventilator_relative_rate", 
            NULL, 
            value = 0.02,
            min = 0.001,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "ventilator_los", 
            NULL, 
            value = 10,
            min = 1,
            max =100
          )
        )
      ),
      #################################
      ## Simulation parameters
      ##################################
      h3("Settings", style = "color:blue"),
      fluidRow(
        column(
          width = 4,
          strong(h4('Forecast days')),
          align='right'
        ),
        column(
          width=4,
          numericInput(
            "future_days", 
            NULL, 
            value = 10,
            min = 8
          )
        ),
        column(
          4,
          actionButton("runButton", "Simulate")
        )
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Observed data",  imageOutput("data")),
        tabPanel("Newly confirmed infected",  imageOutput("new_cases")),
        tabPanel("New demand",                imageOutput("new_demand")),
        tabPanel("Current total demand",      imageOutput("total_demand")),
        tabPanel(
          "Total demand thresholds",
          fluidRow(
            column(
              width=4,
              align='center',
              br(),
              h4('Pr(total hospitalized patients > threshold)'),
              tableOutput("hospital_thresholds")
            ),
            column(
              width=4,
              align='center',
              br(),
              h4('Pr(total ICU (non-ventilator) patients > threshold)'),
              tableOutput("icu_thresholds")
            ),
            column(
              width=4,
              align='center',
              br(),
              h4('Pr(total ventilator patients > threshold)'),
              tableOutput("ventilator_thresholds")
            )
          )
        ),
        # tabPanel("Hospital forecast",         imageOutput("hospital")),
        # tabPanel("ICU forecast",              imageOutput("icu")),
        # tabPanel("Ventilator forecast",       imageOutput("ventilator")),
        tabPanel(
          'Model',
          fluidRow(
            column(
              width = 4,
              align = 'center',
              h3(strong("Doubling time (days)", align="bottom"))
            ),
            column(
              width = 4,
              align = 'center',
              h3(strong("Average days contagious", align="bottom"))
            ),
            column(
              width = 4,
              align = 'center',
              h3(strong("Average days from exposure to infectious", align="bottom"))
            )
          ),
          #------------------
          # Plots 
          #-----------------
          fluidRow(
            column(
              width = 4,
              imageOutput("plot_doubling_time")
            ),
            column(
              width = 4,
              imageOutput("plot_recovery_time")
            ),
            column(
              width = 4,
              imageOutput("plot_exposure_time")
            )
          ),
          br(),
          br(),
          #------------------
          # Prior means
          #-----------------
          fluidRow(
            column(
              width = 2,
              h4('Prior mean'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              numericInput(
                "doubling_mean", 
                label = NULL,
                value = 4.5,
                min = 1
              )
            ),
            column(
              width = 2,
              h4('Prior mean'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              numericInput(
                "recovery_mean", 
                label = NULL,
                value = 10,
                min = 1
              )
            ),
            column(
              width = 2,
              h4('Prior mean'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              numericInput(
                "exposure_mean", 
                label = NULL,
                value = 2.5,
                min = 1
              )
            )
          ),
          #------------------
          # Prior sd
          #-----------------
          fluidRow(
            column(
              width = 2,
              h4('Prior sd'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              numericInput(
                "doubling_sd", 
                label = NULL,
                value = 1.5,
                min = 1
              )
            ),
            column(
              width = 2,
              h4('Prior sd'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              numericInput(
                "recovery_sd", 
                label = NULL,
                value = 2.55,
                min = 1
              )
            ),
            column(
              width = 2,
              h4('Prior sd'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              numericInput(
                "exposure_sd", 
                label = NULL,
                value = 1.5,
                min = 1
              )
            )
          ),
          #------------------
          # Posterior means
          #-----------------
          fluidRow(
            column(
              width = 2,
              h4('Posterior mean'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              h4(textOutput('est_doubling_mean'))
            ),
            column(
              width = 2,
              h4('Posterior mean'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              h4(textOutput('est_recovery_mean'))
            ),
            column(
              width = 2,
              h4('Posterior mean'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              h4(textOutput('est_exposure_mean'))
            )
          ),
          #------------------
          # Posterior sds
          #-----------------
          fluidRow(
            column(
              width = 2,
              h4('Posterior sd'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              h4(textOutput('est_doubling_sd'))
            ),
            column(
              width = 2,
              h4('Posterior sd'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              h4(textOutput('est_recovery_sd'))
            ),
            column(
              width = 2,
              h4('Posterior sd'),
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              h4(textOutput('est_exposure_sd'))
            )
          )
        ),
        tabPanel("Table data",                      tableOutput("contents"))
        # tabPanel("Census forecast", imageOutput("census"))
      )
    )
  )
)