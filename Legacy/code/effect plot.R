library(brms)
library(ggplot2)
library(ggpubr)
library(ggdist)
library(cowplot)
## set wd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## set theme
theme_set(theme_tidybayes() + panel_border())

mydata = read.csv('../data/data_long_RAVLT.csv')
dt_mdl = readRDS('../data/x2min_RAVLT_lvl2in1_full.rds')

# region_level2 = c("parietal")
# region_level1 = c("superiorparietal")
# 
# conditions <- make_conditions(dt_mdl, "Age_yrs")
# conditions$regionlvl2 = region_level2
# conditions$regionlvl1 = region_level1
# 
# plot(conditional_effects(x = dt_mdl, effects = "RAVLT_tot:X2_min", conditions = conditions, re_formula = NULL, prob = 0.7))[[1]] +
#   labs(x= "zRAVLT_tot", y = "zgraymatter", title = sprintf("%s_%s",region_level2, region_level1)) + 
#   geom_rug(sides = "b", inherit.aes = FALSE, data = aggregate(data = mydata, FUN=mean, zgraymatter ~ ID * RAVLT_tot),
#            aes(x = RAVLT_tot, y = zgraymatter), alpha = 0.5, position = position_jitter(width=0.3,height=0)) +
#   theme(axis.title = element_text(size=10), 
#         plot.title = element_text(size=10, hjust = 0.5),
#         strip.text.x = element_text(size = 10),
#         axis.text = element_text(size = 10, colour = "black"))




plot_3way_interaction_RAVLT <- function(region_level2, region_level1){
  conditions <- make_conditions(dt_mdl,"X2_min")
  conditions$regionlvl2 = region_level2
  conditions$regionlvl1 = region_level1
  plot(conditional_effects(x = dt_mdl, effects = "Age_yrs:RAVLT_tot", conditions = conditions, re_formula = NULL, prob = 0.7), line_args = list(se = F))[[1]] +
    labs(x= "zAge_yrs", y = "zgraymatter", title = sprintf("%s_%s",region_level2, region_level1)) + 
    geom_rug(sides = "b", inherit.aes = FALSE, data = aggregate(data = mydata, FUN=mean, zgraymatter ~ ID * Age_yrs),
             aes(x = Age_yrs, y = zgraymatter), alpha = 0.5, position = position_jitter(width=0.3,height=0)) +
    theme(axis.title = element_text(size=10), 
          plot.title = element_text(size=10, hjust = 0.5),
          strip.text.x = element_text(size = 10),
          axis.text = element_text(size = 10, colour = "black"))
  fpath = sprintf("../figure/3way_interaction/%s_%s.png", region_level2, region_level1) 
  ggsave(fpath, dpi = "retina", width = 8, height = 4, units = "in")
  
}


plot_3way_interaction_RAVLT("parietal", "superiorparietal")
plot_3way_interaction_RAVLT("parietal", "inferiorparietal")
plot_3way_interaction_RAVLT("parietal", "posteriorcingulate")
plot_3way_interaction_RAVLT("parietal", "postcentral")





conditions <- make_conditions(dt_mdl,"X2_min")
conditions$regionlvl2 = "parietal"

plot(conditional_effects(x = dt_mdl, effects = "Age_yrs:RAVLT_tot", conditions = conditions, re_formula = NULL, prob = 0.7), line_args = list(se = F))[[1]] +
  labs(x= "zAge_yrs", y = "zgraymatter", title = sprintf("%s","parietal")) + 
  geom_rug(sides = "b", inherit.aes = FALSE, data = aggregate(data = mydata, FUN=mean, zgraymatter ~ ID * Age_yrs),
           aes(x = Age_yrs, y = zgraymatter), alpha = 0.5, position = position_jitter(width=0.3,height=0)) +
  theme(axis.title = element_text(size=10), 
        plot.title = element_text(size=10, hjust = 0.5),
        strip.text.x = element_text(size = 10),
        axis.text = element_text(size = 10, colour = "black"))
fpath = sprintf("../figure/3way_interaction/%s.png", "parietal") 
ggsave(fpath, dpi = "retina", width = 8, height = 4, units = "in")

