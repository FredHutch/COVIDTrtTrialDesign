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
                 numericInput("pct_fp", "% of drugs that are not effective (identical to placebo).", 
                              min = 0, max = 75, value = 0, step = 5),
                 fluidRow(column(10, h4(HTML("Placebo settings"))), align = "center",
                          fluidRow(column(10, numericInput("median_placebo","Median placebo symptom duration (days).",
                                                           min = 5, max = 30, value = 12, step = 1)),
                                   column(10, numericInput("hosp_pl", "Placebo hospitalization probability.", 
                                                           min = 0, max = 0.25, value = 0.1, step = .1))
                          )),
                 fluidRow(column(10, h4(HTML("Effective drug settings"))), align = "center",
                          fluidRow(column(10, sliderInput("eps_inv", 
                                                          "Fold-reduction in median symptom duration (range, trt/drug).", 
                                                          min = 1, max = 1/0.1, step = 0.25, value = c(1, 5))),
                                   column(10, uiOutput("hvar_slider")),
                                   column(10, numericInput("rho_eff", "Treatment effect correlation between duration and hospitalization", 
                                                           min = 0, max = 1, value = 0.6, step = .05))
               ))
               ),
                 mainPanel(plotOutput("distPlot", height = "750", width = "500"))
               )
  ),
  # "Mininum effective HR (ie, HR0)"
  tabPanel("Trial designs",
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(10, h3(HTML("Surrogate design"))), align = "center",
                        fluidRow(
                          column(5, numericInput("n_surrogate", "N per group", min = 5, max = 100, value = 30, step = 5)),
                          column(5, selectInput("alpha_surrogate", "1-sided alpha", 
                                                 choices = c(0.001, 0.01, 0.025, 0.05, 0.1, 0.2), 
                                                 selected = 0.05)),
                          column(10, numericInput("followup_time", "Follow-up (days)", min = 10, max = 60, value = 30, step = 5)),
                          column(10, numericInput("hr_cutoff", p(HTML(paste0("Required effective HR (HR",tags$sub("0"), ")"))), 
                                                 min = 1, max = 3, step = 0.1, value = 1.2))
                        )),
               fluidRow(column(10, h3(HTML("Primary design"))), align = "center",
                        fluidRow(
                          column(5, numericInput("n_primary", "N per group", min = 50, max = 1000, value = 300, step = 50)),
                          column(5, selectInput("alpha_primary", "1-sided alpha", 
                                                choices = c(0.001, 0.01, 0.025, 0.05, 0.1, 0.2), 
                                                selected = 0.05)),
                          column(10, numericInput("rr_cutoff", p(HTML(paste0("Required effective risk ratio (RR",tags$sub("0"), ")"))), 
                                                 min = 1e-6, max = 1, value = 0.9, step = 0.1))
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
                          column(10, numericInput("surrogate_accrual", "Surrogate patient accrual rate/mo ", 
                                                  min = 5, max = 1000, value = 40,
                                                  step = 5)),
                          column(10, numericInput("primary_accrual", "Primary patient accrual rate/mo ", 
                                                  min = 50, max = 1000, value = 400,
                                                  step = 50)),
                          column(10, numericInput("gap_time", "Trial closing periods (mos)", 
                                                  min = 0, max = 6, value = 0.5,
                                                  step = 0.5))
                          ))
             ), 
             mainPanel( plotOutput("generalSeqPlts", height = "600"))
           )
           ),
  tabPanel("Time to efficacious drugs", 
           sidebarLayout(
             sidebarPanel(
               fluidRow(column(10, h4(HTML("Sequential trial sims"))), align = "center",
                        fluidRow(
                          column(5, numericInput("drug_times", "Months btw. candidate drug trial starts", min = 0, max = 6, value = 1,
                                                 step = 0.5)),
                          column(5, numericInput("trial_cens_time", "Unsuccessful trial truncation mo.", min = 12, max = 100, value = 24,
                                                 step = 1)),
                          column(10, numericInput("trial_reps", "Trial simulations", min = 100, max = 5000, value = 250, step = 100))
                        ))
             ), 
             mainPanel(plotOutput("survplot", height = "600", width = "600"))
             )
  ),
  tabPanel("About", 
           h4("Contact: Bryan Mayer (bmayer@fredhutch.org)"),
           div("Code:", a("https://github.com/FredHutch/COVIDTrtTrialDesign"))
           ) 
  )
))