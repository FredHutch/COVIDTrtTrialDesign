library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Treatment trials simulator"),
  
  
  tabsetPanel(
  tabPanel("Drug efficacy distribution", 
             sidebarLayout(
               sidebarPanel(
                 sliderInput("eps_inv", "Fold-reduction in median symptoms (range, 1/surrogate)", 
                             min = 1, max = 1/0.1, step = 0.25, value = c(1, 5)),
                 uiOutput("hvar_slider"),
                 numericInput("rho_eff", "Correlation between surrogate and primary", 
                              min = 0, max = 1, value = 0.6, step = .05),
                 numericInput("median_placebo", "Placebo symptom resolution median (days, surrogate endpoint) ~ Exponential", 
                              min = 5, max = 30, value = 12, step = 1),
                 numericInput("hosp_pl", "Placebo hospitalization probability", 
                              min = 0, max = 1, value = 0.1, step = .1),
                 numericInput("total_drugs", "Total simulated drugs", 
                              min = 1, max = 1000, value = 50, step = 10)
               ),
                 mainPanel(plotOutput("distPlot"))
               )
  ),
  tabPanel("Trial designs",
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(10, strong("Surrogate design")), align = "center",
                        fluidRow(
                          column(5, numericInput("n_surrogate", "N per group", min = 5, max = 500, value = 30, step = 5)),
                          column(5, numericInput("alpha_surrogate", "alpha", min = 0, max = 1, value = 0.1, step = 0.0125)),
                          column(5, numericInput("followup_time", "Follow-up (days)", min = 10, max = 60, value = 30, step = 5))
                        )),
               fluidRow(column(10, strong("Primary design")), align = "center",
                        fluidRow(
                          column(5, numericInput("n_primary", "N per group", min = 5, max = 5000, value = 300, step = 25)),
                          column(5, numericInput("alpha_primary", "alpha", min = 0, max = 1, value = 0.05, step = 0.025))
                        ))
               ),
             mainPanel(plotOutput("distPlot2"))
           )
  ),
  tabPanel("PK3", 
           sidebarLayout(
             sidebarPanel(
               sliderInput(
                 "obs3",
                 "Number of observations:",
                 min = 1,
                 max = 1000,
                 value = 500
               )
             ), 
             mainPanel(plotOutput("distPlot3"))
           )
           )
  )
))