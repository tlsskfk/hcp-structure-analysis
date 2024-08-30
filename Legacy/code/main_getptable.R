library(tidyverse)
library(tidybayes)
library(dplyr)


## set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


getpvalue_1region = function(hold){
  ##SAME AS ABOVE BUT SORT BY EVIDENCE INSTEAD OF ALPHABETICALLY
  my_pvalues <- as.data.frame(matrix(NA, nrow = length(unique(hold$ROI)), ncol = 2))
  my_signs <- aggregate(data=hold, FUN=median, condition_mean ~ ROI)
  my_signs$sign <- sign(my_signs$condition_mean)
  my_signs$condition_mean <- NULL
  hold <- merge(hold, my_signs, by="ROI")
  hold$signed <- hold$condition_mean*hold$sign
  
  colnames(my_pvalues) <- c("ROI","pvalue")
  for (i in 1:length(unique(hold$ROI))){
    my_pvalues[i,1] <- as.character(unique(hold$ROI)[i])
    my_pvalues[i,2] <- length(subset(hold, ROI == unique(hold$ROI)[i] & condition_mean > 0)$condition_mean)/length(subset(hold, ROI == unique(hold$ROI)[i])$condition_mean)
  }
  my_pvalues  <- my_pvalues[order(my_pvalues$pvalue,decreasing=T),]
  # my_pvalues[my_pvalues$pvalue > .15 & my_pvalues$pvalue < .85,]$pvalue <- "NA" #this will be used to make anything with evidence less than 15% gray
  my_pvalues$pvalue <- as.numeric(my_pvalues$pvalue)
  # hold$ROI <- factor(hold$ROI,levels=my_pvalues$ROI)
  # hold <- merge(hold,my_pvalues,by="ROI")
  return(my_pvalues)
}



# m1 = readRDS("./result_COG_R10_lvl1_full_TD12.rds")
# result1 <-  m1 %>%
#   spread_draws(`b_scaleTotal_cog`, `r_regionlvl1b`[ROI, term]) %>%
#   mutate(condition_mean = `b_scaleTotal_cog` + `r_regionlvl1b`)
# 
# hold1 <- result1[result1$term=="scaleTotal_cog",] 
# p1 = getpvalue_1region(hold1)



m1 = readRDS("./result_2min_R10_lvl1_full.rds")

result1 <-  m1 %>%
  spread_draws(`b_scaleX2_min`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleX2_min` + `r_regionlvl1b`)

hold1 <- result1[result1$term=="scaleX2_min",] 

p1 = getpvalue_1region(hold1)
colnames(p1)[2] = "pvalue_2min"
# write.csv(p1, "../pvalue/x2min_pvalue.csv")
# write.csv(ls1[1],"../SH/x2min.csv")



m2 = readRDS("./result_RAVLTXFLANKER_R10_lvl1_full.rds")

result2 <-  m2 %>%
  spread_draws(`b_scaleRAVLT_tot`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleRAVLT_tot` + `r_regionlvl1b`)

hold2 <- result2[result2$term=="scaleRAVLT_tot",] 
p2 = getpvalue_1region(hold2)
colnames(p2)[2] = "pvalue_RAVLT"
# write.csv(ls2[2], "../pvalue/RAVLT_pvalue.csv")
# write.csv(ls2[1],"../SH/RAVLT.csv")





m3 = readRDS("./result_COG_R10_lvl1_full_TD12.rds")

result3 <-  m3 %>%
  spread_draws(`b_scaleTotal_cog`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleTotal_cog` + `r_regionlvl1b`)

hold3 <- result3[result3$term=="scaleTotal_cog",] 
p3 = getpvalue_1region(hold3)
colnames(p3)[2] = "pvalue_COG"
# write.csv(ls3[2], "../pvalue/COG_pvalue.csv")
# write.csv(ls3[1],"../SH/COG.csv")


m5 = readRDS("./result_COG_R10_lvl2_full_TD12.rds")
result5<- m5%>%
  spread_draws(`b_scaleTotal_cog`, `r_regionlvl2b`[ROI2,term],`r_regionlvl2b:regionlvl1b`[ROI1,term]) %>%
  mutate(condition_mean = `b_scaleTotal_cog` + `r_regionlvl2b` + `r_regionlvl2b:regionlvl1b`)
hold5 <- result5[result5$term=="scaleTotal_cog",] 
p5 = getpvalue_1region(hold5)
colnames(p5)[2] = "pvalue_COG_lvl2"


m4 = readRDS("./result_RAVLTXFLANKER_R10_lvl1_full.rds")

result4 <-  m4 %>%
  spread_draws(`b_scaleFlanker`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleFlanker` + `r_regionlvl1b`)

hold4 <- result4[result4$term=="scaleFlanker",] 
p4 = getpvalue_1region(hold4)
colnames(p4)[2] = "pvalue_Flanker"
# write.csv(ls4[2], "../pvalue/Flanker_pvalue.csv")
# write.csv(ls4[1],"../SH/Flanker.csv")

# p1 = p1[p1$pvalue_2min >=0.95,]
# p2 = p2[p2$pvalue_RAVLT >=0.95,]
# p3 = p3[p3$pvalue_COG >= 0.95,]
# p4 = p4[p4$pvalue_Flanker >= 0.95,]

z = merge(p1,p2, by = "ROI")
x = merge(z,p3, by = "ROI")
v = merge(x,p4, by = "ROI")


d= readRDS("./data_brms.RDS")
dt = d[d$ID ==unique(d$ID)[1],]
reg = dt[,c(17,16)]
colnames(reg) = c("ROI", "ROI_LVL2")
ddt = merge(v,reg, by ="ROI")
# ddt$pvalue_2min[ddt$pvalue_2min < 0.99] = NA
a = ddt[,2:5]
idx = a >0.99 | a < 0.01 
tidx = idx == 0
a[tidx] = NA
ddt[,2:5] = a
ddt = arrange(ddt, ddt$ROI_LVL2, desc(ddt$pvalue_2min))
write.csv(ddt, "../pvalue/mastertable.csv")

