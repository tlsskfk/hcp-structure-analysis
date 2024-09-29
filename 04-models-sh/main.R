library(brms)
library(pracma)
library(cmdstanr)

## set wd to here
args <- commandArgs(trailingOnly = FALSE)
script_path <- dirname(normalizePath(sub("--file=", "", args[grep("--file=", args)])))

## Bayesian Model
data = readRDS('./data_brms_log_normalized.RDS')

run_model_prior <- function(md, data, seed = 1, iter_prior = 100, warmup_prior = 10, chains = 4, cores = 16, backend = "cmdstanr"){
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

