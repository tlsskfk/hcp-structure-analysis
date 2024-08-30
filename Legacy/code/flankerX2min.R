# 
d = readRDS("./data_brms.RDS")
# library(readxl)
# flankerX2min = read_excel("../data//ms_99_LVL2__fit_totalcog_4-20-24.xlsx")
# flankerX2min = read.csv("../pvalue/ms_99_LVL2_2minxtotcog.csv")

pmt = read.csv("../pvalue/lvl2.csv")
pmt = pmt[,-1]
pmt = pmt[order(pmt$p_2min2, decreasing = T),]


flankerX2min = pmt[,c(1,2,5)]
roi_flankerX2min = flankerX2min$ROI
idx = d$regionlvl2b %in% roi_flankerX2min
d_flankerX2min = d[idx,]
roi2 = unique(flankerX2min$ROI)
dt = read.csv("../data/Master_CSV_aging_neural.csv")
#scale(RAVLT_tot) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol)
dt = dt[,c(2,6,5,7,22,30,31,33,23,16,112)]
dt = dt[!is.na(dt$Flanker & dt$X2_min),]
dt$Gender = factor(dt$Gender)
dt[,3:(dim(dt)[2])] = scale(dt[,3:(dim(dt)[2])])
get_result_f2 <- function(data){
  model0 = lm(Flanker ~ X2_min + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  modelm = lm(mean_greymatter ~ X2_min + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  modely = lm(Flanker ~ X2_min + mean_greymatter + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  results <- mediate(modelm, modely, treat="X2_min", mediator="mean_greymatter",boot=TRUE, sims=500)
  out = list() 
  out$model0 = model0
  out$modelm = modelm
  out$modely = modely
  out$results = results
  return(out)
}



resls_f2 = matrix(list(), 10,2)
for (i in 1:length(roi2)) {
  print(sprintf("processing %s", roi2[i]))
  resls_f2[[i,1]] = roi2[i]
  d1 = d_flankerX2min[d_flankerX2min$regionlvl2b == roi2[i],]
  t = d1%>%
    group_by(ID) %>%
    summarise(mean_greymatter = mean(graymatter))
  dd = merge(t,dt)
  resls_f2[[i,2]] = get_result_f2(dd)
}

f = function(x, y){
  if (y < 0.001){
    star = '***'
  } else if (y < 0.01){
    star = '**'
  } else if (y < 0.05){
    star = "*"
  }else if (y < 0.1){
    star = '.'
  } else {
    star = " "
  }
  
  str = sprintf('%.4f%s', x, star)
  return(str)
}

out = data.frame(ROI = rep(NA, 1, 10), r1 = rep(NA, 1, 10), r2 = rep(NA, 1, 10), direct = rep(NA, 1, 10), indirect = rep(NA, 1, 10), total = rep(NA, 1, 10))
for (i in 1:length(roi2)) {
  x = resls_f2[[i, 2]]
  a = summary(x$results)
  out$ROI[i] = roi2[i]
  out$r1[i] = f(summary(x$modelm)$coef[2,1], summary(x$modelm)$coef[2,4])
  out$r2[i] = f(summary(x$modely)$coef[3,1], summary(x$modely)$coef[3,4]);
  out$direct[i] = f(a$z0, a$z0.p)
  out$indirect[i] = f(a$d0, a$d0.p)
  out$total[i] = f(a$tau.coef, a$tau.p)
}


out_f2 = merge(flankerX2min, out, by = "ROI")
out_f2 = out_f2[order(out_f2$p_2min2, decreasing  = T),]
write.csv(out_f2, "./out_f2.csv")
