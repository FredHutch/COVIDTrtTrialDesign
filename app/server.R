library(shiny)
library(cobs)
library(tidyverse)
library(viridis)
library(exactci)
library(mvtnorm)
library(survival)
library(survminer)
library(cowplot)


source("trials_funs.R")

lab_eps = "Proportion reduction median symptom duration (surrogate)"
lab_hr = "Placebo vs. treatment symptom duration hazard ratio (HR, surrogate)"
lab_primary = "Trt two-week hospitalization probability  (primary)"
default_hvar = c(0.03, 0.08)

shinyServer(function(input, output) {
  theme_set(theme_bw() + theme(legend.position = "top"))
  text_size = 16
  
  hosp_pl = reactive({input$hosp_pl})
  prop_fp = reactive({input$pct_fp/100})
  output$hvar_slider = renderUI({
    sliderInput("h_var", "Drug hospitalization probability range (primary endpoint)", 
                min = 0, max = hosp_pl(), value = default_hvar, step = 0.01)
  }) 
  
  
  calc_surrogate_power = reactive({
    
    function(eps){
      surrogate_trials_power(
        n = input$n_surrogate,
        median_placebo = input$median_placebo,
        eps = eps,
        hr_cutoff = input$hr_cutoff,
        followup_time = input$followup_time,
        alpha = input$alpha_surrogate
      )
    }
    
  })
  
  calc_primary_power = reactive({
    function(hosp_tx){
      primary_trials_power_sim(
        replicates = input$primary_reps,
        n = input$n_primary,
        hosp_pl = input$hosp_pl,
        hosp_tx = hosp_tx,
        rr_cutoff = input$rr_cutoff,
        alpha = parse_number(input$alpha_primary)
      )
    }
    })
  
  make_primary_power_axis = reactive({
    
    function(input){
      index = seq(1, 1000, by = 20)
      yin = map_dbl(input[index], calc_primary_power())
      monotonic_smoother = cobs(input[index], yin,
               print.warn = F, print.mesg = F,
               constraint= "decrease", degree=1, nknots = 20)
    
    100*predict(monotonic_smoother, interval="none", z=input)[,2]
    
    }
  })

  drug_population = reactive({
    total_eff_drugs = round(input$total_drugs * (1 - prop_fp()))
    total_fp_drugs = round(input$total_drugs * prop_fp())
    
    if(is.null(input$h_var)){
      h_var_range = default_hvar
    } else h_var_range = input$h_var

    stopifnot(total_eff_drugs > 1)
    
    sig = matrix(input$rho_eff, nrow = 2, ncol = 2)
    diag(sig) = 1
    norm_sample = rmvnorm(total_eff_drugs, mean = c(0, 0), sigma = sig)

    eff_tib = tibble(
      surrogate_eff = pnorm(norm_sample[,1]) * (1/input$eps_inv[1] - 1/input$eps_inv[2]) + 1/input$eps_inv[2],
      primary_eff = pnorm(norm_sample[,2]) * (h_var_range[2] - h_var_range[1]) + h_var_range[1]
    )
    fp_tib = tibble(
      surrogate_eff = rep(1, total_fp_drugs),
      primary_eff = rep(hosp_pl(), total_fp_drugs)
    )

    bind_rows(eff_tib, fp_tib) %>%
      rowid_to_column("drug_id")
    
    })
  
  power_res = reactive({
    invisible(input$run_sims)
    drug_population() %>%
      rowwise() %>%
      mutate(
        surrogate = calc_surrogate_power()(eps = surrogate_eff),
        primary = calc_primary_power()(hosp_tx = primary_eff),
        sequential = surrogate * primary
      ) %>%
      ungroup()
  })
  
  trial_duration = reactive({
    trial_duration_calc(n_surrogate = input$n_surrogate, surrogate_accrual = input$surrogate_accrual, 
                        surrogate_fup = input$followup_time, 
                        n_primary = input$n_primary, primary_accrual = input$primary_accrual, 
                        gap_time = input$gap_time) %>%
      mutate(selected = "Selected settings")
    
  })
  
  trial_duration_grid = reactive({
    make_trial_duration_grid(surrogate_accrual = input$surrogate_accrual, 
                        surrogate_fup = input$followup_time, 
                        primary_accrual = input$primary_accrual, 
                        gap_time = input$gap_time) %>%
      mutate(selected = " ")
    
  })
  
  seq_trial_sims = reactive({
    map_df(c(5, 10, 25), function(n_drugs){
      trials_efficacy_time(n_drugs = n_drugs, drug_pwr_dat = power_res(),
                           total_trial_mo = trial_duration()$total_mo, mo_btw_drug = input$drug_times,
                           meta_replicates = input$trial_reps) %>%
        mutate(n_drugs = n_drugs)
    })
  })
  
  output$distPlot = renderPlot({
      ggplot(drug_population(), aes(x = surrogate_eff, y = primary_eff)) +
        geom_point(alpha = 0.5) +
        labs(x = lab_eps, y = lab_primary) +
        theme(text = element_text(size = text_size)) +
        ggtitle(paste(input$total_drugs, "simulated drugs")) +
       geom_vline(xintercept = 1, colour = "black", linetype = "dotted") +
       geom_hline(yintercept =  hosp_pl(), colour = "black", linetype = "dotted")
    })
  
  output$marginalPower = renderPlot({
    ggplot(drug_population(), aes(x = 1/surrogate_eff, y = primary_eff)) +
      geom_point(alpha = 0.5) +
      scale_x_log10(breaks = c(1:5, input$hr_cutoff),
                    sec.axis = sec_axis(~100*calc_surrogate_power()(eps = 1/.), 
                                               breaks = c(5, 10, 50, 80, 90, 99),
                                                 name = "Surrogate power (%)") 
                    ) +
      scale_y_continuous(sec.axis = sec_axis(~make_primary_power_axis()(.), 
                                             breaks = c(5, 10, 50, 80, 90, 99),
                                             name = "Primary power (%)")) +
      geom_vline(xintercept = input$hr_cutoff, colour = "red", linetype = "dashed") +
      geom_vline(xintercept = 1, colour = "black", linetype = "dotted") +
      geom_hline(yintercept = hosp_pl()*input$rr_cutoff, colour = "red", linetype = "dashed") +
      geom_hline(yintercept =  hosp_pl(), colour = "black", linetype = "dotted") +
      labs(x = lab_hr, y = lab_primary) +
      theme(text = element_text(size = text_size)) +
      ggtitle(paste(input$total_drugs, "simulated drugs"))
  })
  
  output$overallPower = renderPlot({
    pwr_breaks = c(5, 10, 50, 80, 90, 99, parse_number(input$alpha_primary) * 100, input$alpha_surrogate * 100)
    trial_types = c("surrogate", "primary", "sequential")
    
    power_res() %>%
      pivot_longer(cols = trial_types, names_to = "trial_type", values_to = "value") %>%
      mutate(
        value = 100 * value,
        trial_type = factor(trial_type, levels = trial_types,
                            labels = str_to_title(trial_types))) %>%
      ggplot(aes(x = trial_type, y = value)) +
      geom_boxplot() +
      geom_point(alpha = 0.5) +
      geom_path(aes(group = drug_id), alpha = 0.25) +
      labs(x = "Trial type", y = "Power (%)") +
      theme(text = element_text(size = text_size)) +
      ggtitle(paste(input$total_drugs, "simulated drugs")) +
      scale_y_continuous(breaks = pwr_breaks) 
  })  
  
  output$generalSeqPlts = renderPlot({
    grid_pl = trial_duration_grid() %>%
      ggplot(aes(x = n_primary, y = total_mo, shape = selected)) +
      geom_line(aes(colour = factor(n_surrogate))) +
      geom_point(data = trial_duration(), fill = "black", size = 2.5) +
      scale_y_continuous(breaks = 0:12) +
      scale_x_continuous(breaks = (0:5) * 200) +
      coord_cartesian(ylim = c(0, 12)) +
      scale_shape_manual("", breaks = c(" ", "Selected settings"), values = c(NA,15)) +
      labs(x = "Primary N", y = "Successful trial duration (months)",
           colour = "Surrogate N", fill = "") +
      theme(text = element_text(size = text_size - 2)) 
    
    duration_barpl = trial_duration() %>%
      mutate(
        gap_surrogate = input$gap_time,
        post_primary = input$gap_time
      ) %>%
      dplyr::select(surrogate_mo, primary_mo, gap_surrogate, post_primary) %>%
      gather() %>%
      mutate(
        key = factor(key, 
                     levels = c("surrogate_mo", "gap_surrogate", "primary_mo", "post_primary"),
                     labels = c("Surrogate", "Surrogate Close", "Primary", "Primary Close"))
      ) %>%
      arrange(key) %>%
      mutate(
        cumulative_value = cumsum(value) - value/2,
        key = fct_rev(key)
        ) %>%
      ggplot((aes(x = " ", y = value, fill = key, label = key))) +
      geom_bar(stat = "identity", show.legend = F) +
      geom_label(aes(y = cumulative_value, colour = key), x = 1.5, vjust = 1, show.legend = F, fill = "white") +
      scale_color_viridis(guide = "none", discrete = T) +
      scale_fill_viridis(guide = "none", discrete = T) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, trial_duration()$total_mo + 3), breaks = (0:12) * 2) +
      scale_x_discrete(expand = c(0, 0.5)) +
      coord_flip() +
      labs(x = "Selected settings", y = "Total duration (months)") +
      theme_classic() +
      theme(text = element_text(size = text_size - 2)) 
    
    plot_grid(grid_pl, duration_barpl, rel_heights = c(10, 3), nrow = 2)
      
  })
  
  output$survplot = renderPlot({
    plot_stem = "Cum. incidence >="
    
    trial_sim_long = seq_trial_sims() %>%
      gather(threshold, months, time_1drug:time_3drug) %>%
      mutate(
        obs = if_else(months <= input$trial_cens_time, T, F),
        month_trunc = pmin(months, input$trial_cens_time)
        )
    
    surv_plots = map(unique(trial_sim_long$threshold), function(i){
      tmp = filter(trial_sim_long, threshold == i)
      fit <- survfit(Surv(month_trunc, obs) ~ n_drugs, data = tmp)
      pl = ggsurvplot(fit, tmp, fun = "event", conf.int = F,
                      ylab = paste0(plot_stem, parse_number(i), " drug"),
                      break.time.by = 2, 
                      break.y.by = 0.2, ylim = c(0,1),
                      palette = "jco", pval = F) 
      pl$plot +
        geom_hline(yintercept = c(0.8, 0.9), linetype = "dashed") +
        labs(x = "Months") +
        theme(axis.text.x = element_text(size = 7),
              axis.text.y = element_text(size = 9))
    }) 
    
    plot_grid(plotlist = surv_plots[1:3], nrow = 3)
    
    
  })
  
})
