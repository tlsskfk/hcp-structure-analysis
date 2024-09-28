library(tidyverse)
library(tidybayes)
library(dplyr)
library(mediation)

## set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# 
d = readRDS("./data_brms.RDS")
# library(readxl)
# cogx2min = read_excel("../data//ms_99_LVL2__fit_totalcog_4-20-24.xlsx")
# cogx2min = read.csv("../pvalue/ms_99_LVL2_2minxtotcog.csv")

mt = read.csv("../pvalue/mastertable.csv")
mt = mt[,-1]


cogx2min = mt[,c(1,2,4,6)]
cogx2min = na.omit(cogx2min)
roi_cogx2min = cogx2min$ROI
idx = d$regionlvl1b %in% roi_cogx2min
d_cogx2min = d[idx,]
roi2 = unique(d_cogx2min$regionlvl2)
dt = read.csv("../data/Master_CSV_aging_neural.csv")
#scale(Age_yrs) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol)
dt = dt[,c(2,6,5,7,22,30,23,16,112)]
dt = dt[!is.na(dt$X2_min & dt$Total_cog),]
dt$Gender = factor(dt$Gender)
dt[,3:(dim(dt)[2])] = scale(dt[,3:(dim(dt)[2])])

get_result_c2 <- function(data){
  model0 = lm(Total_cog ~ X2_min + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  modelm = lm(mean_greymatter ~ X2_min + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  modely = lm(Total_cog ~ X2_min + mean_greymatter + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  results <- mediate(modelm, modely, treat="X2_min", mediator="mean_greymatter",boot=TRUE, sims=500)
  out = list() 
  out$model0 = model0
  out$modelm = modelm
  out$modely = modely
  out$results = results
  return(out)
}


r1 = roi2[1]
d1 = d_cogx2min[d_cogx2min$regionlvl2 == r1,]
t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd = merge(t,dt)
res1 = get_result_c2(dd)



r2 = roi2[2]
d2 = d_cogx2min[d_cogx2min$regionlvl2 == r2,]
t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd2 = merge(t2,dt)
res2 = get_result_c2(dd2)



r3 = roi2[3]
d3 = d_cogx2min[d_cogx2min$regionlvl2 == r3,]
t3 = d3%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd3 = merge(t3,dt)
res3 = get_result_c2(dd3)



r4 = roi2[4]
d4 = d_cogx2min[d_cogx2min$regionlvl2 == r4,]
t4 = d4%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd4 = merge(t4,dt)
res4 = get_result_c2(dd4)




summary(res1$model0)
summary(res1$modelm)
summary(res1$modely)
summary(res1$results)


summary(res2$model0)
summary(res2$modelm)
summary(res2$modely)
summary(res2$results)


summary(res3$model0)
summary(res3$modelm)
summary(res3$modely)
summary(res3$results)


summary(res4$model0)
summary(res4$modelm)
summary(res4$modely)
summary(res4$results)





r1 = roi2[1]
d1 = d_cogx2min[d_cogx2min$regionlvl2 == r1,]
t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd = merge(t,dt)
res1 = get_result_c2(dd)


################RAVLT###################



cogxravlt = mt[,c(1,2,3,6)]
dt = read.csv("../data/Master_CSV_aging_neural.csv")
#scale(RAVLT_tot) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol)
dt = dt[,c(2,6,5,7,22,30,33,23,16,112)]
dt = dt[!is.na(dt$RAVLT_tot & dt$X2_min),]
dt$Gender = factor(dt$Gender)
dt[,3:(dim(dt)[2])] = scale(dt[,3:(dim(dt)[2])])
get_result_cr <- function(data){
  model0 = lm(RAVLT_tot ~ X2_min + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  modelm = lm(mean_greymatter ~ X2_min + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  modely = lm(RAVLT_tot ~ X2_min + mean_greymatter + Age_yrs + Gender + BMI 
              + walk_pace + Education_yrs + EstimatedTotalIntraCranialVol, data)
  results <- mediate(modelm, modely, treat="X2_min", mediator="mean_greymatter",boot=TRUE, sims=500)
  out = list() 
  out$model0 = model0
  out$modelm = modelm
  out$modely = modely
  out$results = results
  return(out)
}


cogxravlt = na.omit(cogxravlt)

roi_cogxravlt = cogxravlt$ROI
idx = d$regionlvl1b %in% roi_cogxravlt
d_cogxravlt = d[idx,]
roi2 = unique(d_cogxravlt$regionlvl2)
r1 = roi2[1]
d1 = d_cogxravlt[d_cogxravlt$regionlvl2 == r1,]
t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd = merge(t,dt)
rres1 = get_result_cr(dd)

r2 = roi2[2]
d2 = d_cogxravlt[d_cogxravlt$regionlvl2 == r2,]
t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd2 = merge(t2,dt)
rres2 = get_result_cr(dd2)

summary(rres1$model0)
summary(rres1$modelm)
summary(rres1$modely)
summary(rres1$results)


summary(rres2$model0)
summary(rres2$modelm)
summary(rres2$modely)
summary(rres2$results)





###############################
cogxflanker = mt[,c(1,2,5,6)]
dt = read.csv("../data/Master_CSV_aging_neural.csv")
#scale(RAVLT_tot) + Gender + scale(BMI) + scale(walk_pace) + scale(Education_yrs) + scale(EstimatedTotalIntraCranialVol)
dt = dt[,c(2,6,5,7,22,30,31,33,23,16,112)]
dt = dt[!is.na(dt$Flanker & dt$X2_min),]
dt$Gender = factor(dt$Gender)
dt[,3:(dim(dt)[2])] = scale(dt[,3:(dim(dt)[2])])
get_result_cf <- function(data){
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


cogxflanker = na.omit(cogxflanker)

roi_cogxflanker = cogxflanker$ROI
idx = d$regionlvl1b %in% roi_cogxflanker
d_cogxflanker = d[idx,]
roi2 = unique(d_cogxflanker$regionlvl2)
r1 = roi2[1]
d1 = d_cogxflanker[d_cogxflanker$regionlvl2 == r1,]
t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd = merge(t,dt)
fres1 = get_result_cf(dd)

r2 = roi2[2]
d2 = d_cogxflanker[d_cogxflanker$regionlvl2 == r2,]
t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd2 = merge(t2,dt)
fres2 = get_result_cf(dd2)


r3 = roi2[3]
d3 = d_cogxflanker[d_cogxflanker$regionlvl2 == r3,]
t3 = d3%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))
dd3 = merge(t3,dt)
fres3 = get_result_cf(dd3)

summary(fres1$model0)
summary(fres1$modelm)
summary(fres1$modely)
summary(fres1$results)


summary(fres2$model0)
summary(fres2$modelm)
summary(fres2$modely)
summary(fres2$results)


summary(fres3$model0)
summary(fres3$modelm)
summary(fres3$modely)
summary(fres3$results)
















# Install packages
# install.packages("psych")
# install.packages("lavaan")
# install.packages("ggplot2")
# install.packages("readxl")
# install.packages("semPlot")

# Load packages
library(psych)
library(lavaan)
library(ggplot2)
library(readxl)
library(semPlot)


mediation_model <- '
  # Direct effects
  mean_greymatter ~ a * X2_min
  Total_cog ~ c * X2_min + b * mean_greymatter

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect

'

dd[,2:4] = scale(dd[,2:4])
# Estimate the mediation model
mediation_results <- sem(mediation_model, data = dd)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results, whatLabels = "est", style = "lisrel", intercepts = FALSE)




r2 = roi2[2]
d2 = d_cogx2min[d_cogx2min$regionlvl2 == r2,]

t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd2 = merge(t2,dt)

dd2[,2:4] = scale(dd2[,2:4])



# Estimate the mediation model
mediation_results2 <- sem(mediation_model, data = dd2)

# Summarize the results
a = summary(mediation_results2, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results2, whatLabels = "est", style = "lisrel", intercepts = FALSE)







r3 = roi2[3]
d3 = d_cogx2min[d_cogx2min$regionlvl2 == r3,]

t3 = d3%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd3 = merge(t3,dt)
dd3[,2:4] = scale(dd3[,2:4])




# Estimate the mediation model
mediation_results3 <- sem(mediation_model, data = dd3)

# Summarize the results
summary(mediation_results3, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results3, whatLabels = "est", style = "lisrel", intercepts = FALSE)




r4 = roi2[4]
d4 = d_cogx2min[d_cogx2min$regionlvl2 == r4,]

t4 = d4%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd4 = merge(t4,dt)
dd4[,2:4] = scale(dd4[,2:4])

# Estimate the mediation model
mediation_results4 <- sem(mediation_model, data = dd4)

# Summarize the results
summary(mediation_results4, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results4, whatLabels = "est", style = "lisrel", intercepts = FALSE)










# Load the necessary libraries
library(ggplot2)
path = c("Indirect", "M->Y", "Total", "X->M","X->Y" )
coefficient = c(0.053,14.484,0.3,0.004,0.247)
path_data = data.frame(path, coefficient)

# Create a bar plot to visualize the path coefficients
ggplot(path_data, aes(x = path, y = coefficient, fill = path)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(coefficient, 3)), vjust = -0.3, size = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Coefficient") +
  xlab("Path") +
  ggtitle("Mediation Analysis Results")



# Load the necessary libraries
library(ggplot2)
library(semPlot)

# Create a bar plot to visualize the path coefficients
bar_plot <- ggplot(path_data, aes(x = path, y = coefficient, fill = path)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(coefficient, 3)), vjust = -0.3, size = 4) +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Coefficient") +
  xlab("Path") +
  ggtitle("Mediation Analysis Results")

# Plot the bar plot
print(bar_plot)

# Plot the mediation diagram with path estimates
semPaths(mediation_results, whatLabels = "est", style = "lisrel", intercepts = FALSE)









######################

ravltx2min = read.csv("../pvalue/ms_99_LVL2_2minxravlt.csv")

roi_ravltx2min = ravltx2min$ROI
idx = d$regionlvl1b %in% roi_ravltx2min
d_ravltx2min = d[idx,]
roi2 = unique(d_ravltx2min$regionlvl2)
r1 = roi2[1]
d1 = d_ravltx2min[d_ravltx2min$regionlvl2 == r1,]

t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dt = read.csv("../data/Master_CSV_aging_neural.csv")
dt = dt[,c(2,22,30)]

dd = merge(t,dt)




mediation_model <- '
  # Direct effects
  mean_greymatter ~ a * X2_min
  RAVLT_tot ~ c * X2_min + b * mean_greymatter

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect

'

dd[,2:4] = scale(dd[,2:4])
# Estimate the mediation model
mediation_results <- sem(mediation_model, data = dd)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results, whatLabels = "est", style = "lisrel", intercepts = FALSE)




r2 = roi2[2]
d2 = d_ravltx2min[d_ravltx2min$regionlvl2 == r2,]

t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd2 = merge(t2,dt)

dd2[,2:4] = scale(dd2[,2:4])



# Estimate the mediation model
mediation_results2 <- sem(mediation_model, data = dd2)

# Summarize the results
a = summary(mediation_results2, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results2, whatLabels = "est", style = "lisrel", intercepts = FALSE)




###############################
d = readRDS("./data_brms.RDS")

roi2 = unique(d$regionlvl2)
r1 = roi2[1]
d1 = d[d$regionlvl2 == r1,]

t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd = merge(t,dt)


# model0 = lm(Total_cog ~ X2_min, dd)
# summary(model0)
# modelm = lm(mean_greymatter ~ X2_min, dd)
# summary(modelm)
# modely = lm(Total_cog ~ X2_min + mean_greymatter, dd)
# summary(modely)
# 
# install.packages("mediation")
# library(mediation)
# results <- mediate(modelm, modely, treat='X2_min', mediator='mean_greymatter',
#                    boot=TRUE, sims=500)
# summary(results)
# 

# Load packages
library(psych)
library(lavaan)
library(ggplot2)
library(readxl)
library(semPlot)


mediation_model <- '
  # Direct effects
  mean_greymatter ~ a * X2_min
  Total_cog ~ c * X2_min + b * mean_greymatter

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect

'

dd[,2:4] = scale(dd[,2:4])
# Estimate the mediation model
mediation_results <- sem(mediation_model, data = dd)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results, whatLabels = "est", style = "lisrel", intercepts = FALSE)




r2 = roi2[2]
d2 = d[d$regionlvl2 == r2,]

t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd2 = merge(t2,dt)

dd2[,2:4] = scale(dd2[,2:4])



# Estimate the mediation model
mediation_results2 <- sem(mediation_model, data = dd2)

# Summarize the results
summary(mediation_results2, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results2, whatLabels = "est", style = "lisrel", intercepts = FALSE)







r3 = roi2[3]
d3 = d[d$regionlvl2 == r3,]

t3 = d3%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd3 = merge(t3,dt)
dd3[,2:4] = scale(dd3[,2:4])




# Estimate the mediation model
mediation_results3 <- sem(mediation_model, data = dd3)

# Summarize the results
summary(mediation_results3, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results3, whatLabels = "est", style = "lisrel", intercepts = FALSE)




r4 = roi2[4]
d4 = d[d$regionlvl2 == r4,]

t4 = d4%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd4 = merge(t4,dt)
dd4[,2:4] = scale(dd4[,2:4])

# Estimate the mediation model
mediation_results4 <- sem(mediation_model, data = dd4)

# Summarize the results
summary(mediation_results4, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results4, whatLabels = "est", style = "lisrel", intercepts = FALSE)





r5 = roi2[5]
d5 = d[d$regionlvl2 == r5,]

t5 = d5%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd5 = merge(t5,dt)
dd5[,2:4] = scale(dd5[,2:4])

# Estimate the mediation model
mediation_results5 <- sem(mediation_model, data = dd5)

# Summarize the results
summary(mediation_results5, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results4, whatLabels = "est", style = "lisrel", intercepts = FALSE)





# 
# d = readRDS("./data_brms.RDS")
library(readxl)
cogx2min = read.csv("../pvalue/test2.csv")
roi_cogx2min = cogx2min$ROI
idx = d$regionlvl1b %in% roi_cogx2min
d_cogx2min = d[idx,]
roi2 = unique(d_cogx2min$regionlvl2)
r1 = roi2[1]
d1 = d_cogx2min[d_cogx2min$regionlvl2 == r1,]

t = d1%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dt = read.csv("../data/Master_CSV_aging_neural.csv")
dt = dt[,c(2,22,30)]

dd = merge(t,dt)


# model0 = lm(Total_cog ~ X2_min, dd)
# summary(model0)
# modelm = lm(mean_greymatter ~ X2_min, dd)
# summary(modelm)
# modely = lm(Total_cog ~ X2_min + mean_greymatter, dd)
# summary(modely)
# 
# install.packages("mediation")
# library(mediation)
# results <- mediate(modelm, modely, treat='X2_min', mediator='mean_greymatter',
#                    boot=TRUE, sims=500)
# summary(results)
# 





# Load packages
library(psych)
library(lavaan)
library(ggplot2)
library(readxl)
library(semPlot)


mediation_model <- '
  # Direct effects
  mean_greymatter ~ a * X2_min
  Total_cog ~ c * X2_min + b * mean_greymatter

  # Indirect effect (a * b)
  indirect := a * b

  # Total effect (c + indirect)
  total := c + indirect

'

dd[,2:4] = scale(dd[,2:4])
# Estimate the mediation model
mediation_results <- sem(mediation_model, data = dd)

# Summarize the results
summary(mediation_results, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results, whatLabels = "est", style = "lisrel", intercepts = FALSE)




r2 = roi2[2]
d2 = d_cogx2min[d_cogx2min$regionlvl2 == r2,]

t2 = d2%>%
  group_by(ID) %>%
  summarise(mean_greymatter = mean(graymatter))

dd2 = merge(t2,dt)

dd2[,2:4] = scale(dd2[,2:4])



# Estimate the mediation model
mediation_results2 <- sem(mediation_model, data = dd2)

# Summarize the results
summary(mediation_results2, standardized = TRUE, fit.measures = TRUE)
semPaths(mediation_results2, whatLabels = "est", style = "lisrel", intercepts = FALSE)







