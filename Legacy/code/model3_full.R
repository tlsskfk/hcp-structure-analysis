library(brms)
library(pracma)

## set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Bayesian Model
data = readRDS('./data_brms.RDS')

run_model_prior <- function(md, data, seed = 1, iter_prior = 100, warmup_prior = 10, chains = 4, cores = 4, backend = "cmdstanr"){
  command_prior = sprintf('tomodpriors = brm(%s, data = data, backend = backend,chains = chains, iter = iter_prior, warmup = warmup_prior, seed = seed, cores = cores, control=list(adapt_delta=0.99, max_treedepth = 15))', md)   
  print(command_prior)
  eval(parse(text = command_prior))
  
  
  #extract list of default priors to then modify as desired
  mypriors <- prior_summary(tomodpriors)
  mypriors
  
  #setting priors:
  mypriors[dim(mypriors)[1],1] <- sprintf("cauchy(0,%.2f)", std(data$graymatter)) #set residual prior to half cauchy with scale matching sd of zgretmatter
  mypriors[mypriors$class=="sd",1] <- "" #weakly informative variance/covariance as per Chen et al. 2019
  
  output = list()
  output$model = md
  output$mypriors = mypriors
  output$data = data
  output$backend = backend
  output$chains = chains
  return(output)
}

run_model = function(temp,iter=4000,warmup= 1000, ...){
  tic()
  command <- sprintf('result = brm(%s, control=list(adapt_delta=0.99, max_treedepth = 15), prior = temp$mypriors, data = temp$data, chains = temp$chains, iter = iter, warmup = warmup, backend = temp$backend, ...)', temp$model)
  eval(parse(text = command))
  result$elapsetime = toc()
  return(result)
}




md = "graymatter~ scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp = run_model_prior(md, data = data) 
result = run_model(temp, cores = 10, threads = threading(2), file = './result_COG_R10_lvl1_full.rds')


md2 = "graymatter~ scale(X2_min) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(X2_min) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp2 = run_model_prior(md2, data = data) 
result = run_model(temp2, cores = 10, threads = threading(2), file = './result_2min_R10_lvl1_full.rds')

md3 = "graymatter~ scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp3 = run_model_prior(md3, data = data) 
result = run_model(temp3, cores = 10, threads = threading(2), file = './result_RAVLT_tot_R10_lvl1_full.rds')


md4 = "graymatter~ scale(Flanker) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Flanker) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp4 = run_model_prior(md4, data = data) 
result = run_model(temp4, cores = 10, threads = threading(2), file = './result_Flanker_R10_lvl1_full.rds')


md5 = "graymatter~ scale(Flanker) + scale(Total_cog) + scale(X2_min) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Flanker) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp5 = run_model_prior(md5, data = data) 
result = run_model(temp5, cores = 10, threads = threading(2), file = './result_all_R10_lvl1_full.rds')



md6 = "graymatter~ scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp6 = run_model_prior(md6, data = data) 
result6 = run_model(temp6, cores = 10, threads = threading(2), file = './result_COG_R10_lvl1_full_TD12.rds')


{
md7 = "graymatter~ scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)" 
temp7 = run_model_prior(md7, data = data) 
result7 = run_model(temp7, cores = 10, threads = threading(2), file = './result_COG_R10_lvl2_full_TD12.rds')

}


md8 = "graymatter~ scale(Flanker) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Flanker) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
temp8 = run_model_prior(md8, data = data) 
result = run_model(temp8, cores = 10, threads = threading(2), file = './result_RAVLTXFLANKER_R10_lvl1_full.rds')



{
  md9 = "graymatter~ scale(Flanker) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(Flanker) + scale(RAVLT_tot) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)" 
  temp9 = run_model_prior(md9, data = data) 
  result = run_model(temp9, cores = 10, threads = threading(2), file = './result_RAVLTXFLANKER_R10_lvl2_full.rds')
  
}



{
  md10 = "graymatter~ scale(  X2_min  ) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  X2_min  ) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +            (1 | ID)" 
  temp10 = run_model_prior(md10, data = data) 
  result10 = run_model(temp10, cores = 10, threads = threading(2), file = './result_2min_R10_lvl2_full_TD12.rds')
  
}


{
  md11 = "graymatter~ scale(  X2_min  ) * scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  X2_min  ) * scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +  (1 | ID)" 
  temp11 = run_model_prior(md11, data = data) 
  result11 = run_model(temp11, cores = 10, threads = threading(2), file = './result_agex2min_R10_lvl2_full_TD15.rds')
  
}



{
  md12 = "graymatter~ scale(  Total_cog  ) * scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  Total_cog  ) * scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl2b/regionlvl1b) +  (1 | ID)" 
  temp12 = run_model_prior(md12, data = data) 
  result12 = run_model(temp12, cores = 10, threads = threading(2), file = './result_agextotcog_R10_lvl2_full_TD15.rds')
  
}


###############################
run_model_prior2 <- function(md, data, seed = 1, iter_prior = 100, warmup_prior = 10, chains = 4, cores = 4 ){
  command_prior = sprintf('tomodpriors = brm(%s, data = data, chains = chains, iter = iter_prior, warmup = warmup_prior, seed = seed, cores = cores)', md)   
  print(command_prior)
  eval(parse(text = command_prior))
  
  
  #extract list of default priors to then modify as desired
  mypriors <- prior_summary(tomodpriors)
  mypriors
  
  #setting priors:
  mypriors[dim(mypriors)[1],1] <- sprintf("cauchy(0,%.2f)", std(data$graymatter)) #set residual prior to half cauchy with scale matching sd of zgretmatter
  mypriors[mypriors$class=="sd",1] <- "" #weakly informative variance/covariance as per Chen et al. 2019
  
  output = list()
  output$model = md
  output$mypriors = mypriors
  output$data = data
  output$chains = chains
  return(output)
}
run_model2 = function(temp,iter=4000,warmup= 1000, ...){
  tic()
  command <- sprintf('result = brm(%s, control = list(adapt_delta = 0.99), prior = temp$mypriors, data = temp$data, chains = temp$chains, iter = iter, warmup = warmup, ...)', temp$model)
  eval(parse(text = command))
  result$elapsetime = toc()
  return(result)
}

md = "graymatter~ scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) +
             (1 + scale(  Total_cog  ) + scale(Age_yrs) + Gender + scale(BMI)   + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol) | regionlvl1b) +            (1 | ID)" 
btemp = run_model_prior2(md, data = data) 
bresult = run_model2(btemp, cores = 10, threads = threading(2), file = './result_COG_R10_lvl1_full_nocmd.rds')
