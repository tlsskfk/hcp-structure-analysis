md_FnR = "graymatter~ scale(Flanker) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Flanker) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)"
temp_FnR = run_model_prior(md_FnR, data = data)
result_FnR = run_model(temp_FnR, cores = 16, threads = threading(1), file = './result_RAVLTXFLANKER_lvl2_R10_TD12.rds')