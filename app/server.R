library(shiny)
library(mvtnorm)
library(dplyr)
library(tidyr)
library(purrr)
library(pracma)
library(viridis)
library(ggplot2)
library(cowplot)

shinyServer(function(input, output) {
  theme_set(theme_bw() + theme(legend.position = "top"))
  text_size = 16

  hosp_pl = reactive({input$hosp_pl})
  
  output$hvar_slider = renderUI({
    sliderInput("h_var", "Drug hospitalization probability range (primary endpoint)", 
                min = 0, max = hosp_pl(), value = c(0.03, 0.08), step = 0.01)
  }) 
  
  norm_sample = reactive({
    sig = matrix(input$rho_eff, nrow = 2, ncol = 2)
    diag(sig) = 1
    norm_sample = rmvnorm(input$total_drugs, mean = c(0, 0), sigma = sig)
    
    tibble(
      surrogate_eff = pnorm(norm_sample[,1]) * (1/input$eps_inv[1] - 1/input$eps_inv[2]) + 1/input$eps_inv[2],
      primary_eff = pnorm(norm_sample[,2]) * (input$h_var[2] - input$h_var[1]) + input$h_var[1]
    )
    
    })
  
  power_res = reactive({
    
  })
  
  output$distPlot = renderPlot({
    lab_x = "Proportion reduction median symptom duration (surrogate)"
    lab_y = "Two-week hospitalization probability  (primary)"
    (
      ggplot(norm_sample(), aes(x = surrogate_eff, y = primary_eff)) +
        geom_point(alpha = 0.5) +
        labs(x = lab_x, y = lab_y) +
        theme(text = element_text(size = text_size)) +
        ggtitle(paste(input$total_drugs, "simulated drugs"))
    )  %>%
      ggExtra::ggMarginal(type = "histogram")
  })
  

  
  output$distPlot2 <- renderPlot({
    # generate an rnorm distribution and plot it
    dist <- rnorm(input$obs2)
    hist(dist)
  })
  
  output$distPlot3 <- renderPlot({
    # generate an rnorm distribution and plot it
    dist <- rnorm(input$obs3)
    hist(dist)
  })
  
})
