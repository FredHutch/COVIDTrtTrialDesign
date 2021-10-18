# ------------ Surrogate trial functions -----------

surrogate_logrank = function(surrogate_trial) {
  
  survdiff(Surv(symptom_time, event) ~ fct_relevel(group, "placebo"), data = surrogate_trial)
  
}

surrogate_cox = function(surrogate_trial) {
  coxph(Surv(symptom_time, event) ~ fct_relevel(group, "placebo"),
        data = surrogate_trial)
}


surrogate_trials_power = function(n, median_placebo = 12, eps = 1, hr_cutoff = 1.2, followup_time = 30, alpha = 0.05){
  # http://powerandsamplesize.com/Calculators/Test-Time-To-Event-Data/Cox-PH-1-Sided-non-inferiority-superiority
  n_total = n * 2 # note n is per group
  pA = 0.5 # these are the pcts of total n to each group, fixed for now but documents
  hr0 = hr_cutoff
  hr = 1/eps
  lambda_0 = log(2) / median_placebo
  lambda = log(2) / (median_placebo * eps)
  pE = 0.5*(1 - exp(-lambda_0 * followup_time)) + 0.5*(1 - exp(-lambda * followup_time))
  
  pnorm((log(hr)-log(hr0))*sqrt(n_total*pA*(1-pA)*pE)-qnorm(1-alpha))
}

run_surrogate_trial = function(n, median_placebo = 12, eps = 1, followup_time = 30, ceil = T) {
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

surrogate_trials_power_sim = function(replicates,
                                  n,
                                  median_placebo = 12,
                                  eps = 1,
                                  hr_cutoff = 1,
                                  alpha = 0.05,
                                  followup_time = 30) {
  map_lgl(1:replicates, function(i) {
    sim_data = run_surrogate_trial(n, median_placebo, eps, followup_time)
    lr_mod = surrogate_logrank(sim_data)
    sim_mod = surrogate_cox(sim_data)
    hr_lower = exp(confint(sim_mod, level = (1 - alpha * 2)))[1]
    (hr_lower > hr_cutoff) &
      ((lr_mod$obs - lr_mod$exp)[1] < 0) & (broom::glance(lr_mod)$p.value < alpha * 2)
  }) %>%
    mean()
}

# ------------ Primary trial functions -----------


primary_trials_power_sim = function(replicates,
                                    n,
                                    hosp_pl = 0.1,
                                    hosp_tx = 0.05,
                                    rr_cutoff = 1,
                                    alpha = 0.05) {
  map_lgl(1:replicates, function(i) {
    primary_res = poisson.exact(
      x = c(rbinom(1, n, hosp_tx), rbinom(1, n, hosp_pl)),
      T = c(n, n),
      conf.level = 1 - alpha,
      alternative = "less"
    )
    primary_res$conf.int[2] < rr_cutoff
  }) %>%
    mean()
}

# ------------ Trial duration -------------


trial_duration_mo = function(n, monthly_accrual, followup_days) n*2/monthly_accrual + followup_days/30.4375

trial_duration_calc = function(n_surrogate, surrogate_accrual, surrogate_fup, 
                               n_primary, primary_accrual, primary_fup = 15, 
                               gap_time){
  tibble(
    n_surrogate = n_surrogate,
    n_primary = n_primary,
    surrogate_mo = trial_duration_mo(n_surrogate, monthly_accrual = surrogate_accrual, followup_days = surrogate_fup),
    primary_mo = trial_duration_mo(n_primary, monthly_accrual = primary_accrual, followup_days = primary_fup),
    total_mo = surrogate_mo + primary_mo + 2 * gap_time
  )
}

make_trial_duration_grid = function(surrogate_accrual, surrogate_fup, primary_accrual, primary_fup = 15, gap_time){
  
  crossing(
    n_surrogate = c(10, 30, 50, 100),
    n_primary = c(5, 50, 100, 300, 600, 1000)
  ) %>%
    rowwise() %>%
    transmute(
      trial_dur_dat = list(trial_duration_calc(
        n_surrogate = n_surrogate, surrogate_accrual = surrogate_accrual, surrogate_fup = surrogate_fup,
        n_primary = n_primary, primary_accrual = primary_accrual, primary_fup = primary_fup,
        gap_time = gap_time)
      )
    ) %>%
    unnest(cols = trial_dur_dat) %>%
    ungroup()

}

# ------------ Multiple drugs -------------

# inputs are matched vectors
calc_time_to_efficacy = function(trial_complete_time, success, drug_threshold = 1){
  if(sum(success) < drug_threshold) return(Inf)
  cumulative_success = cumsum(success)
  trial_complete_time[which(cumulative_success == drug_threshold)[1]]
}

trials_efficacy_time = function(n_drugs, drug_pwr_dat, total_trial_mo, mo_btw_drug = 1, meta_replicates = 1){
  map_df(1:meta_replicates, function(i){
    dplyr::slice_sample(drug_pwr_dat, n = n_drugs) %>% 
      transmute(
        success = rbinom(n_drugs, size = 1, prob = surrogate*primary),
        trial_complete_time = total_trial_mo + mo_btw_drug * (0:(n_drugs - 1))
      ) %>%
      summarize(
        meta_sim = i,
        time_1drug = calc_time_to_efficacy(trial_complete_time, success, drug_threshold = 1),
        time_2drug = calc_time_to_efficacy(trial_complete_time, success, drug_threshold = 2),
        time_3drug = calc_time_to_efficacy(trial_complete_time, success, drug_threshold = 3)
      )
    })
}




