md_cog = "graymatter~ scale(Total_cog) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Total_cog) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)"
temp_cog = run_model_prior(md_cog, data = data)
result_cog = run_model(temp_cog, cores = 16, threads = threading(1), file = '../06-bayes-results/result_COG_R10_lvl2_TD12.rds')