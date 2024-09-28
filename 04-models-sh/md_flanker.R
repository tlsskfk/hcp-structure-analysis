md_flanker = "graymatter~ scale(Flanker) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Flanker) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)"
temp_flanker = run_model_prior(md_flanker, data = data)
result_flanker = run_model(temp_flanker, cores = 16, threads = threading(1), file = './result_FLANKER_R10_lvl2_TD12.rds')