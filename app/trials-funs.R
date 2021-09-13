# ------------ Trial data simulators -----------

surrogate_logrank = function(surrogate_trial) {
  
  survdiff(Surv(symptom_time, event) ~ fct_relevel(group, "placebo"), data = surrogate_trial)
  
}

surrogate_cox = function(surrogate_trial) {
  coxph(Surv(symptom_time, event) ~ fct_relevel(group, "placebo"),
        data = surrogate_trial)
}


run_surrogate_trial = function(n,
                               median_placebo = 12,
                               eps = 1,
                               followup_time = 30,
                               ceil = T) {
  out = tibble(
    group = rep(c("placebo", "trt"), each = n),
    symptom_time = pmin(followup_time, c(
      rexp(n, log(2) / median_placebo), rexp(n, log(2) / (median_placebo * eps))
    )),
    event = if_else(symptom_time < followup_time, 1, 0)
  )
  if (ceil)
    out$symptom_time = ceiling(out$symptom_time)
  out
}


run_primary_trial = function(n, hosp_pl = 0.1, hosp_tx = 0.05){
  tibble(
    group = rep(c("placebo", "trt"), each = n),
    numhosp = c(rbinom(n, 1, hosp_pl), rbinom(n, 1, hosp_tx)) 
  )
}




# ---------- Simulation functions --------------

surrogate_trials_simulater = function(replicates, n, median_placebo = 12, eps = 1, hr_cutoff = 1.2,alpha = 0.05,
                                      followup_time = 15, monthly_accrual = 40, keep_sim_data = F){
  map_df(1:replicates, function(i){
    sim_data = run_surrogate_trial(n, median_placebo, eps, followup_time)
    lr_mod = surrogate_logrank(sim_data)
    sim_mod = surrogate_cox(sim_data)
    hr_lower = exp(confint(sim_mod, level = (1 - alpha * 2)))[1]
    
    output = tibble(
      sim = i,
      trial_months = n*2/monthly_accrual + followup_time/30.4375, # from EK's code (patient accrual + final ppt follow up)
      logrank_chisq = lr_mod$chisq,
      logrank_stat_placebo = (lr_mod$obs - lr_mod$exp)[1],
      logrank_pvalue = broom::glance(lr_mod)$p.value,
      logrank_test = if_else(logrank_stat_placebo < 0, logrank_pvalue < alpha * 2, F),
      hr = exp(broom::tidy(sim_mod)[['estimate']]),
      hr_lower = hr_lower,
      hr_test = hr_lower > hr_cutoff
    )
    output
    
  })
}



sequential_trials_simulator = function(replicates,
                                       n_surrogate,
                                       median_placebo = 12,
                                       eps = 1,
                                       hr_cutoff = 1.2,
                                       alpha_surrogate = 0.05,
                                       followup_time = 15,
                                       monthly_accrual = 40,
                                       n_primary,
                                       hosp_pl = 0.1,
                                       hosp_tx = 0.5,
                                       hosp_rr_cutoff = 0.9,
                                       alpha_primary = 0.025,
                                       keep_sim_data = F) {
  
  
  map_df(1:replicates, function(i) {
    surrogate_outcome = surrogate_trials_simulater(
      replicates = 1,
      n = n_surrogate,
      median_placebo = median_placebo,
      eps = eps,
      hr_cutoff = hr_cutoff,
      alpha = alpha_surrogate,
      followup_time = followup_time,
      monthly_accrual = monthly_accrual,
      keep_sim_data = keep_sim_data
    ) %>%
      mutate(sim = i) # there is a separate sim iterator in this function that would just repeat 1
    
    # break if surrogate trial fails
    if(!surrogate_outcome$logrank_test | !surrogate_outcome$hr_test){
      return(mutate(surrogate_outcome, primary_rr = NA, primary_upperCI = NA, primary_outcome = NA))
    }
    
    primary_sim_raw = run_primary_trial(n = n_primary, hosp_pl = hosp_pl, hosp_tx = hosp_tx)
    primary_sim = primary_sim_raw %>%
      group_by(group) %>%
      summarize(
        n = n(),
        total_hosp = sum(numhosp)
      ) %>%
      pivot_wider(names_from = "group",
                  values_from = c("n", "total_hosp"))
    
    primary_res = poisson.exact(x = c(primary_sim$total_hosp_trt, primary_sim$total_hosp_placebo),
                                T = c(primary_sim$n_trt, primary_sim$n_placebo),
                                conf.level = 1 - alpha_primary, alternative = "less")
    
    if(keep_sim_data) surrogate_outcome$primary_data = list(primary_sim_raw)
    
    mutate(surrogate_outcome,  primary_rr = primary_res[['estimate']], 
           primary_upperCI = primary_res$conf.int[2], primary_outcome = primary_res$conf.int[2] < hosp_rr_cutoff)
    
  })
  
}

summarize_primary = function(sequential_trials_res){
  sequential_trials_res %>%
    summarize(
      total = n(),
      avg_hr = exp(mean(log(hr))),
      primary_total = sum(logrank_test & hr_test),
      primary_power = mean(logrank_test & hr_test),
      avg_rr = if_else(primary_total > 0, exp(mean(log(primary_rr[logrank_test & hr_test]))), NA_real_),
      success_power = if_else(primary_total > 0, sum(primary_outcome, na.rm = T)/total, 0)
    )
}