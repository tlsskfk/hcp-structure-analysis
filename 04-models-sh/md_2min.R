md_2min = "graymatter~ scale(  X2_min  ) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  X2_min  ) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)"
temp_2min = run_model_prior(md_2min, data = data)
result_2min = run_model(temp_2min, cores = 16, threads = threading(1), file = './result_2min_R10_lvl2_TD12.rds')