md_ravlt = "graymatter~scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)"
temp_ravlt = run_model_prior(md_ravlt, data = data)
result_ravlt = run_model(temp_ravlt, cores = 16, threads = threading(1), file = './result_RAVLT_R10_lvl2_TD12.rds')