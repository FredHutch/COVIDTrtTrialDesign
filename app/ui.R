library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Treatment trials simulator"),
  
  
  tabsetPanel(
  tabPanel("Drug efficacy distribution", 
             sidebarLayout(
               sidebarPanel(
                 numericInput("total_drugs", "Total simulated drugs.", 
                              min = 1, max = 1000, value = 50, step = 25),
                 sliderInput("eps_inv", "Fold-reduction in median symptoms (range, 1/surrogate).", 
                             min = 1, max = 1/0.1, step = 0.25, value = c(1, 5)),
                 numericInput(
                   "median_placebo",
                   "Placebo symptom resolution median (days, surrogate null endpoint) ~ Exponential.",
                   min = 5, max = 30, value = 12, step = 1),
                 uiOutput("hvar_slider"),
                 numericInput("hosp_pl", "Placebo hospitalization probability (null primary).", 
                              min = 0, max = 0.25, value = 0.1, step = .1),
                 numericInput("rho_eff", "Correlation between effective surrogate and primary.", 
                              min = 0, max = 1, value = 0.6, step = .05),
                 numericInput("pct_fp", "% of drugs that are not effective.", 
                              min = 0, max = 75, value = 0, step = 5)
               ),
                 mainPanel(plotOutput("distPlot"))
               )
  ),
  # "Mininum effective HR (ie, HR0)"
  tabPanel("Trial designs",
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(10, h3(HTML("Surrogate design"))), align = "center",
                        fluidRow(
                          column(10, sliderInput("n_surrogate", "N per group", min = 5, max = 100, value = 30, step = 5)),
                          column(5, numericInput("alpha_surrogate", "alpha", min = 0.01, max = 1, value = 0.1, step = 0.01)),
                          column(5, numericInput("followup_time", "Follow-up (days)", min = 10, max = 60, value = 30, step = 5)),
                          column(10, sliderInput("hr_cutoff", p(HTML(paste0("Mininum effective HR (HR",tags$sub("0"), ")"))), 
                                                 min = 1, max = 3, step = 0.1, value = 1.2))
                        )),
               fluidRow(column(10, h3(HTML("Primary design"))), align = "center",
                        fluidRow(
                          column(10, sliderInput("n_primary", "N per group", min = 50, max = 1000, value = 300, step = 50)),
                          column(10, selectInput("alpha_primary", "alpha", 
                                                choices = c(0.001, 0.01, 0.025, 0.05, 0.1), 
                                                selected = 0.05)),
                          column(10, sliderInput("rr_cutoff", p(HTML(paste0("Maximum  effective risk ratio (RR",tags$sub("0"), ")"))), 
                                                 min = 0.5, max = 1, value = 0.9, step = 0.1))
                        )),
               fluidRow(column(10, h4(HTML("Primary trial sim controls"))), align = "center",
                        fluidRow(
                          column(10, numericInput("primary_reps", "Total sims", min = 100, max = 5000, value = 500, step = 100)),
                          column(10, actionButton("run_sims", "Re-run sims"))
                        ))
               ),
             mainPanel(
               tabsetPanel(
                 tabPanel("Trial power by drug", plotOutput("marginalPower")),
                 tabPanel("Distribution of power", plotOutput("overallPower"))
               )
             )
           )
  ),
  tabPanel("Trial durations", 
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(10, h4(HTML("Trial time controls"))), align = "center",
                        fluidRow(
                          column(10, numericInput("surrogate_accrual", "Surrogate patient accrual rate/mo ", min = 5, max = 1000, value = 40,
                                                  step = 5)),
                          column(10, numericInput("primary_accrual", "Primary patient accrual rate/mo ", min = 50, max = 1000, value = 400,
                                                  step = 50)),
                          column(10, numericInput("gap_time", "Trial closing periods (mos)", min = 0, max = 6, value = 0.5,
                                                  step = 0.5))
                          )),
               fluidRow(column(10, h4(HTML("Sequential trial sims"))), align = "center",
                        fluidRow(
                          column(5, numericInput("drug_times", "Months btw. drugs", min = 0, max = 6, value = 1,
                                                 step = 0.5)),
                          column(5, numericInput("trial_cens_time", "Functional censor mo.", min = 12, max = 100, value = 24,
                                                 step = 1)),
                          column(10, numericInput("trial_reps", "Trial simulations", min = 100, max = 5000, value = 250, step = 100))
                        ))
             ), 
             mainPanel(
               tabsetPanel(
                 tabPanel("Sequential trial durations", plotOutput("generalSeqPlts")),
                 tabPanel("Time to efficacious drugs", plotOutput("survplot", height = "600", width = "600"))
               )
             )
           )
           )
  )
))