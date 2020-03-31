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
        "Proportion of newly exposed people that eventually test positive", 
        value = 0.375,
        min = 0.0001,
        max = 1
      ),
      numericInput(
        "lag", 
        "Time from exposure to positive test result", 
        value = 19,
        min = 0.0,
        max = 30
      ),
      numericInput(
        "social", 
        "Relative reduction in transmission (social distancing)", 
        value = 0,
        min = 0.00,
        max = 0.999999
      ),
      em('Social distancing begins taking effect on 03/13/20 (day after school closure) and takes full effect on 3/19/20.'),
      #################################      
      ## Demand parameters Parameters
      ##################################
      h5(strong("Proportion of newly exposed people that become:", align="bottom")),
      fluidRow(
        column(
          3,
          numericInput(
            "hospital_relative_rate", 
            h5("Hospitalized"), 
            value = 0.1,
            min = 0.001,
            max = 1
          )
        ),
        column(
          3,
          numericInput(
            "icu_relative_rate", 
            h5("ICU"), 
            value = 0.03,
            min = 0.001,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "ventilator_relative_rate", 
            h5("Ventilator"), 
            value = 0.02,
            min = 0.001,
            max = 100
          )
        )
      ),
      #################################      
      ## Demand parameters Parameters
      ##################################
      h5(strong("Average length of stay by demand type:", align="bottom")),
      fluidRow(
        column(
          3,
          numericInput(
            "hospital_los", 
            h5("Hospitalized"), 
            value = 7,
            min = 1,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "icu_los", 
            h5("ICU"), 
            value = 9,
            min = 9,
            max = 100
          )
        ),
        column(
          3,
          numericInput(
            "ventilator_los", 
            h5("Ventilator"), 
            value = 10,
            min = 1,
            max = 100
          )
        )
      ),
      #################################
      ## Simulation parameters
      ##################################
      h3("Settings", style = "color:blue"),
      fluidRow(
        column(
          width = 5,
          strong(h4('Forecast length')),
          align='right'
        ),
        column(
          width=3,
          numericInput(
            "future_days", 
            NULL, 
            value = 42,
            min = 8
          )
        ),
        column(
          3,
          actionButton("runButton", "Simulate")
        )
      ),
      radioButtons(
        "dataSet", 
        h3("Data", style = "color:blue"), 
        choices = list(
          "DPH" = 'dph', 
          "DPH (last 10)" = 'dph_last_10', 
          "EMS" = 'ems',
          "EMS (PUI)" = 'pui'
        ),
        selected = 'dph_last_10'
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Observed data",  imageOutput("data")),
        tabPanel("Newly exposed individuals",  imageOutput("new_cases")),
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
              h4('Pr(total ICU patients > threshold)'),
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
              h3(strong("R0", align="bottom"))
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
              imageOutput("plot_R0")
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
              '',
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              ''
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
                value = 1.0,
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
              '',
              align='right'
            ),
            column(
              width = 2,
              align = 'left',
              ''
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
              h4(textOutput('est_r0_mean'))
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
              h4(textOutput('est_r0_sd'))
            )
          )
        ),
        tabPanel("Table data",                      tableOutput("contents"))
        # tabPanel("Census forecast", imageOutput("census"))
      )
    )
  )
)