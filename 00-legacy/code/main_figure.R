library(tidybayes)
library(dplyr)
library(tidyverse)
library(ggplot2)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

plot_posterior_1region = function(hold, savename = NULL,title = ""){
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
  my_pvalues  <- my_pvalues[order(my_pvalues$pvalue,decreasing=FALSE),]
  # my_pvalues[my_pvalues$pvalue > .15 & my_pvalues$pvalue < .85,]$pvalue <- "NA" #this will be used to make anything with evidence less than 15% gray
  my_pvalues$pvalue <- as.numeric(my_pvalues$pvalue)
  hold$ROI <- factor(hold$ROI,levels=my_pvalues$ROI)
  hold <- merge(hold,my_pvalues,by="ROI")
  mpg$class = with(mpg, reorder(class, hwy, median))
  hold$ROI = with(hold,reorder(ROI,condition_mean,median))
  
  myplot <- hold  %>%
    # arrange(condition_mean) %>%
    ggplot(aes(y = ROI, x = condition_mean, fill = pvalue)) +
    stat_halfeye(.width=c(.9,.95)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_fill_viridis_c(option="H", direction = -1, limits=c(0,1), breaks=c(1,.85,.15,0), values=c(1,.85,.15,0), na.value="lightgrey",
                         guide = guide_colorbar(reverse=FALSE, barheight = unit(8,"cm")),name = "P+") +
    labs(title = title) +
    # coord_cartesian(xlim = c(-0.01, 0.01)) +
    theme(axis.title = element_text(size=12), 
          plot.title = element_text(size=12),
          axis.text = element_text(size = 10, colour = "black"))
  if (!is.null(savename)){
    ggsave(savename,dpi = "retina", width = 2337, height = 1653*4, units = "px",bg = "white")
    
  }
  return(myplot)
}

m1 = readRDS("./result_2min_R10_lvl1_full.rds")

result1 <-  m1 %>%
  spread_draws(`b_scaleX2_min`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleX2_min` + `r_regionlvl1b`)

hold1 <- result1[result1$term=="scaleX2_min",] 

savename = "../figure/X2_min_level1.png"
title = "X2_min_level1"
plot_posterior_1region(hold1,savename, title)


m2 = readRDS("./result_RAVLTXFLANKER_R10_lvl1_full.rds")

result2 <-  m2 %>%
  spread_draws(`b_scaleRAVLT_tot`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleRAVLT_tot` + `r_regionlvl1b`)

hold2 <- result2[result2$term=="scaleRAVLT_tot",] 

savename2 = "../figure/RAVLT_tot_level1.png"
title2 = "RAVLT_level1"
plot_posterior_1region(hold2,savename2, title2)




m3 = readRDS("./result_COG_R10_lvl1_full_TD12.rds")

result3 <-  m3 %>%
  spread_draws(`b_scaleTotal_cog`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleTotal_cog` + `r_regionlvl1b`)

hold3 <- result3[result3$term=="scaleTotal_cog",] 

savename3 = "../figure/COG_tot_level1.png"
title3 = "COG_level1"
plot_posterior_1region(hold3,savename3, title3)





m4 = m2

result4 <-  m4 %>%
  spread_draws(`b_scaleFlanker`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleFlanker` + `r_regionlvl1b`)

hold4 <- result4[result4$term=="scaleFlanker",] 

savename4 = "../figure/Flanker_level1.png"
title4 = "FLANKER_level1"
plot_posterior_1region(hold4,savename4, title4)









################### 2 levels ########################
plot_posterior_2regions <- function(hold, savename = NULL,title = ""){
  ##SAME AS ABOVE BUT SORT BY EVIDENCE INSTEAD OF ALPHABETICALLY
  
  my_pvalues <- as.data.frame(matrix(NA, nrow = length(unique(hold$ROI1)), ncol = 2))
  
  colnames(my_pvalues) <- c("ROI1","pvalue")
  for (i in 1:length(unique(hold$ROI1))){
    my_pvalues[i,1] <- as.character(unique(hold$ROI1)[i])
    my_pvalues[i,2] <- length(subset(hold, ROI1 == unique(hold$ROI1)[i] & condition_mean > 0)$condition_mean)/length(subset(hold, ROI1 == unique(hold$ROI1)[i])$condition_mean)
  }
  my_pvalues  <- my_pvalues[order(my_pvalues$pvalue,decreasing=FALSE),]
  # my_pvalues[my_pvalues$pvalue > .15 & my_pvalues$pvalue < .85,]$pvalue <- "NA" #this will be used to make anything with evidence less than 15% gray
  my_pvalues$pvalue <- as.numeric(my_pvalues$pvalue)
  hold$ROI1 <- factor(hold$ROI1,levels=my_pvalues$ROI1)
  hold <- merge(hold,my_pvalues,by="ROI1")
  mpg$class = with(mpg, reorder(class, hwy, median))
  hold$ROI1 = with(hold,reorder(ROI1,condition_mean,median))
  
  myplot <- hold  %>%
    # arrange(condition_mean) %>%
    ggplot(aes(y = ROI1, x = condition_mean, fill = pvalue)) +
    stat_halfeye(.width=c(.9,.95)) +
    geom_vline(xintercept = c(0), linetype = "dashed") +
    scale_fill_viridis_c(option="H", direction = -1, limits=c(0,1), breaks=c(1,.85,.15,0), values=c(1,.85,.15,0), na.value = "lightgrey",
                         guide = guide_colorbar(reverse=FALSE, barheight = unit(8,"cm"), draw.ulim = F,draw.llim = F,name = "P+")) +
    labs(title = title)+
    # coord_cartesian(xlim = c(-0.01, 0.01)) +
    theme(axis.title = element_text(size=9), 
          plot.title = element_text(size=9),
          axis.text = element_text(size = 9, colour = "black"))
  if (!is.null(savename)){
    ggsave(savename,dpi = "retina", width = 2337, height = 1653*1.5, units = "px",bg = "white")
    
  }
  return(myplot)
  
}



result2<- m1%>%
  spread_draws(`b_scaleX2_min:scaleRAVLT_tot`, `r_regionlvl2`[ROI2,term],`r_regionlvl2:regionlvl1`[ROI1,term]) %>%
  mutate(condition_mean = `b_scaleX2_min:scaleRAVLT_tot` + `r_regionlvl2` + `r_regionlvl2:regionlvl1`)


#now a bunch of work to get only the rows where the higher-level parcel is the same as the lower-level:
result2$labellength <- nchar(result2$ROI2)
result2$doesitmatch <- substr(result2$ROI1,1,result2$labellength)
result2$match <- 0
result2[result2$ROI2==result2$doesitmatch,]$match <- 1
result2 <- subset(result2, match == 1)

result2[result2$term=="scaleX2_min:scaleRAVLT_tot",]  %>%
  ggplot(aes(y = ROI1, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")

hold2 <- result2[result2$term=="scaleX2_min:scaleRAVLT_tot",] 
savename2 = "../figure/RAVLT/lvl1/scaleX2_min_scaleRAVLT_tot_level1.png"
title2 = "X2_min_RAVLT_level1"
plot_posterior_2regions(hold2,savename2,title2)



result_match <- function(result2){
  result2$labellength <- nchar(result2$ROI2)
  result2$doesitmatch <- substr(result2$ROI1,1,result2$labellength)
  result2$match <- 0
  result2[result2$ROI2==result2$doesitmatch,]$match <- 1
  result2 <- subset(result2, match == 1)
  return(result2)
}



m5 = readRDS("./result_COG_R10_lvl2_full_TD12.rds")

result5<- m5%>%
  spread_draws(`b_scaleTotal_cog`, `r_regionlvl2b`[ROI,term]) %>%
  mutate(condition_mean = `b_scaleTotal_cog` + `r_regionlvl2b`)

result5[result5$term=="scaleTotal_cog",]  %>%
  ggplot(aes(y = ROI2, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")



hold5 <- result5[result5$term=="scaleTotal_cog",] 
savename5 = "../figure/totconlvl2.png"
title5 = "totalCOG_level2"
plot_posterior_1region(hold5,savename5,title5)




result6<- m5%>%
  spread_draws(`b_scaleTotal_cog`, `r_regionlvl2b`[ROI2,term],`r_regionlvl2b:regionlvl1b`[ROI1,term]) %>%
  mutate(condition_mean = `b_scaleTotal_cog` + `r_regionlvl2b` + `r_regionlvl2b:regionlvl1b`)


#now a bunch of work to get only the rows where the higher-level parcel is the same as the lower-level:
result6 = result_match(result6)

result6[result6$term=="scaleTotal_cog",]  %>%
  ggplot(aes(y = ROI1, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")

hold6 <- result6[result6$term=="scaleTotal_cog",] 
savename6 = "../figure/totconlvl1.png"
title6 = "totalCOG_level1"
plot_posterior_2regions(hold6,savename6,title6)




m7 = readRDS("./result_2min_R10_lvl2_full_TD12.rds")
result7<- m7%>%
  spread_draws(`b_scaleX2_min`, `r_regionlvl2b`[ROI,term]) %>%
  mutate(condition_mean = `b_scaleX2_min` + `r_regionlvl2b`)

result7[result7$term=="scaleX2_min",]  %>%
  ggplot(aes(y = ROI, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")



hold7 <- result7[result7$term=="scaleX2_min",] 
savename7 = "../figure/2minlvl2.png"
title7 = "2min_level2"
plot_posterior_1region(hold7,savename7,title7)




result8<- m7%>%
  spread_draws(`b_scaleX2_min`, `r_regionlvl2b`[ROI2,term],`r_regionlvl2b:regionlvl1b`[ROI1,term]) %>%
  mutate(condition_mean = `b_scaleX2_min` + `r_regionlvl2b` + `r_regionlvl2b:regionlvl1b`)


#now a bunch of work to get only the rows where the higher-level parcel is the same as the lower-level:
result8 = result_match(result8)

result8[result8$term=="scaleX2_min",]  %>%
  ggplot(aes(y = ROI1, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")

hold8 <- result8[result8$term=="scaleX2_min",] 
savename8 = "../figure/2minlvl1.png"
title8 = "2minlvl1"
plot_posterior_2regions(hold8,savename8,title8)




m9 = readRDS("./result_RAVLTXFLANKER_R10_lvl2_full.rds")
result9<- m9%>%
  spread_draws(`b_scaleRAVLT_tot`, `r_regionlvl2b`[ROI,term]) %>%
  mutate(condition_mean = `b_scaleRAVLT_tot` + `r_regionlvl2b`)

result9[result9$term=="scaleRAVLT_tot",]  %>%
  ggplot(aes(y = ROI, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")



hold9 <- result9[result9$term=="scaleRAVLT_tot",] 
savename9 = "../figure/RAVLTlvl2.png"
title9 = "RAVLT_level2"
plot_posterior_1region(hold9,savename9,title9)




result10<- m9%>%
  spread_draws(`b_scaleRAVLT_tot`, `r_regionlvl2b`[ROI2,term],`r_regionlvl2b:regionlvl1b`[ROI1,term]) %>%
  mutate(condition_mean = `b_scaleRAVLT_tot` + `r_regionlvl2b` + `r_regionlvl2b:regionlvl1b`)


#now a bunch of work to get only the rows where the higher-level parcel is the same as the lower-level:
result10 = result_match(result10)

result10[result10$term=="scaleRAVLT_tot",]  %>%
  ggplot(aes(y = ROI1, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")

hold10 <- result10[result10$term=="scaleRAVLT_tot",] 
savename10 = "../figure/RAVLTlvl1.png"
title10 = "RAVLT_level1"
plot_posterior_2regions(hold10,savename10,title10)






result11<- m9%>%
  spread_draws(`b_scaleFlanker`, `r_regionlvl2b`[ROI,term]) %>%
  mutate(condition_mean = `b_scaleFlanker` + `r_regionlvl2b`)

result11[result11$term=="scaleFlanker",]  %>%
  ggplot(aes(y = ROI, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")



hold11 <- result11[result11$term=="scaleFlanker",] 
savename11 = "../figure/Flankerlvl2.png"
title11 = "Flanker_level2"
plot_posterior_1region(hold11,savename11,title11)




result12<- m9%>%
  spread_draws(`b_scaleFlanker`, `r_regionlvl2b`[ROI2,term],`r_regionlvl2b:regionlvl1b`[ROI1,term]) %>%
  mutate(condition_mean = `b_scaleFlanker` + `r_regionlvl2b` + `r_regionlvl2b:regionlvl1b`)


#now a bunch of work to get only the rows where the higher-level parcel is the same as the lower-level:
result12 = result_match(result12)

result12[result12$term=="scaleFlanker",]  %>%
  ggplot(aes(y = ROI1, x = condition_mean)) +
  stat_halfeye(.width=c(.9,.95)) + geom_vline(xintercept = c(0), linetype = "dashed")

hold12 <- result12[result12$term=="scaleFlanker",] 
savename12 = "../figure/Flankerlvl1.png"
title12 = "Flanker_level1"
plot_posterior_2regions(hold12,savename12,title12)



################################################
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


p_totcog2 = getpvalue_1region(hold5)
colnames(p_totcog2)[2] = "p_totcog2"

p_2min2 = getpvalue_1region(hold7)
colnames(p_2min2)[2] = "p_2min2"

p_ravlt2 = getpvalue_1region(hold9)
colnames(p_ravlt2)[2] = "p_ravlt2"

p_flanker2 = getpvalue_1region(hold11)
colnames(p_flanker2)[2] = "p_flanker2"

z = merge(p_2min2,p_totcog2, by = "ROI")
x = merge(z,p_ravlt2, by = "ROI")
v = merge(x,p_flanker2, by = "ROI")



getpvalue_2regions = function(hold){
  ##SAME AS ABOVE BUT SORT BY EVIDENCE INSTEAD OF ALPHABETICALLY
  my_pvalues <- as.data.frame(matrix(NA, nrow = length(unique(hold$ROI1)), ncol = 2))
  my_signs <- aggregate(data=hold, FUN=median, condition_mean ~ ROI1)
  my_signs$sign <- sign(my_signs$condition_mean)
  my_signs$condition_mean <- NULL
  hold <- merge(hold, my_signs, by="ROI1")
  hold$signed <- hold$condition_mean*hold$sign
  
  colnames(my_pvalues) <- c("ROI1","pvalue")
  for (i in 1:length(unique(hold$ROI1))){
    my_pvalues[i,1] <- as.character(unique(hold$ROI1)[i])
    my_pvalues[i,2] <- length(subset(hold, ROI1 == unique(hold$ROI1)[i] & condition_mean > 0)$condition_mean)/length(subset(hold, ROI1 == unique(hold$ROI1)[i])$condition_mean)
  }
  my_pvalues  <- my_pvalues[order(my_pvalues$pvalue,decreasing=T),]
  # my_pvalues[my_pvalues$pvalue > .15 & my_pvalues$pvalue < .85,]$pvalue <- "NA" #this will be used to make anything with evidence less than 15% gray
  my_pvalues$pvalue <- as.numeric(my_pvalues$pvalue)
  # hold$ROI <- factor(hold$ROI,levels=my_pvalues$ROI)
  # hold <- merge(hold,my_pvalues,by="ROI")
  return(my_pvalues)
}

p_totcog1 = getpvalue_2regions(hold6)
colnames(p_totcog1)[2] = "p_totcog1"

p_2min1 = getpvalue_2regions(hold8)
colnames(p_2min1)[2] = "p_2min1"

p_ravlt1 = getpvalue_2regions(hold10)
colnames(p_ravlt1)[2] = "p_ravlt1"

p_flanker1 = getpvalue_2regions(hold12)
colnames(p_flanker1)[2] = "p_flanker1"

z1 = merge(p_2min1,p_totcog1, by = "ROI1")
x1 = merge(z1,p_ravlt1, by = "ROI1")
v1 = merge(x1,p_flanker1, by = "ROI1")

