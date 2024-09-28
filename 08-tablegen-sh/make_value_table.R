library(tidybayes)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)

# Set working directory to script's location
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Use setwd above for rstudio,  use the below for Rscript --save using the command line in bash
args <- commandArgs(trailingOnly = FALSE)
script_path <- dirname(normalizePath(sub("--file=", "", args[grep("--file=", args)])))


#Function to map a numeric value to a color using the viridis 'H' option
map_value_to_color <- function(value, min_value, max_value) {
  # Normalize value to range [0, 1]
  normalized_value <- (value - min_value) / (max_value - min_value)
  
  # Ensure the value is within [0, 1]
  normalized_value <- max(min(normalized_value, 1), 0)
  
  # Get the corresponding color from the viridis color scale
  color_hex <- viridis(256, option = "H", direction = -1)[max(min(floor(normalized_value * 255) + 1, 256), 1)]
  
  # Return the RGB representation of the color
  return(col2rgb(color_hex))
}

getpvlaue_1region = function(hold){
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
  p_table <- hold %>%
    group_by(ROI) %>%
    summarise(p_value = first(pvalue))
  return(list(hold, p_table))
}

#### Read in data
m1 = readRDS("./result_RAVLTXFLANKER_R10_lvl1_full.rds")
str(m1)

result1 <-  m1 %>%
  spread_draws(`b_scaleX2_min`, `r_regionlvl1b`[ROI, term]) %>%
  mutate(condition_mean = `b_scaleX2_min` + `r_regionlvl1b`)

# Calculate hold table and p value table
holdXpval = getpvlaue_1region(result1[result1$term=="scaleX2_min",] )
hold = holdXpval[1]
my_pvalues = holdXpval[[2]]

# Get rgb table
rgb_color <- matrix(nrow = length(my_pvalues$p_value), ncol = 3)
for (i in 1:length(my_pvalues$p_value)){
  test <- map_value_to_color(my_pvalues$p_value[i], 1, 0)
  rgb_color[i, 1:3] <- test
}
# Print the RGB color
print(rgb_color)

my_pvalues$R=rgb_color[,1]
my_pvalues$G=rgb_color[,2]
my_pvalues$B=rgb_color[,3]

# Convert the list to a dataframe
df <- as.data.frame(my_pvalues)

# Now apply the filter operation
lh_table <- df %>% dplyr::filter(grepl("^lh_", ROI))
rh_table <- df %>% dplyr::filter(grepl("^rh_", ROI))



write.csv(file="lh_mypvalues_scaleX2_min.csv", lh_table)
write.csv(file="rh_mypvalues_scaleX2_min.csv", rh_table)




#####
library(tidyverse)
library(dplyr)
p_table <- dt %>%
  group_by(ROI) %>%
  summarise(p_value = first(pvalue))



savename = "../test.png"
title = "test"
plot_posterior_1region(hold1,savename, title)

