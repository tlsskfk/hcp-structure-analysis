library(tidyverse)
library(pracma)
library(caret)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## functions 
scaleall <- function(x0){
  x = as.matrix(x0)
  x = (x - mean(x))/sd(x)
  return(x)
}

W.str_selectbetweenpattern <- function(str, s1, s2, n1, n2){
  require(stringr)
  if (is.null(s1)){
    i1 = c(0,0)
  }else {
    i1 = str_locate_all(str, fixed(s1))
    if (n1 < 0){
      n1 = dim(i1[[1]])[1] + 1 + n1
    }
    if (dim(i1[[1]])[1] == 0){
      return(str)
    }
    i1 = i1[[1]][n1,]
  }
  if (is.null(s2)){
    i2 = c(str_length(str)+1, str_length(str)+1)
  } else{
    i2 = str_locate_all(str, fixed(s2))
    if (n2 < 0){
      n2 = dim(i2[[1]])[1] + 1 + n2
    }
    if (dim(i2[[1]])[1] == 0){
      return(str)
    }
    i2 = i2[[1]][n2,]
  }
  out = substr(str, i1[2]+1, i2[1]-1)
  return(out)
}


W.strs_selectbetweenpattern <- function(strs, s1, s2, n1, n2){
  require(pracma)
  out = arrayfun(function(x)W.str_selectbetweenpattern(x,s1,s2,n1,n2), strs)
  return(out)
}


# ## load raw data
dataraw = read.csv("../data/Master_CSV_aging_neural.csv")


## extract data and change data names
# brain volume
data_vol = dataraw %>%
  select("ID", "Left.Thalamus.Proper", "Left.Caudate", "Left.Putamen", "Left.Pallidum","Left.Hippocampus",
         "Left.Amygdala","Left.Accumbens.area","Left.VentralDC",
         "Right.Thalamus.Proper", "Right.Caudate","Right.Putamen","Right.Pallidum",
         "Right.Hippocampus","Right.Amygdala","Right.Accumbens.area","Right.VentralDC")
cnames = names(data_vol)
cnames = paste(gsub("Left.", "lh_", cnames), "subcortical", sep = '_')
cnames = gsub("Right.", "rh_", cnames)
cnames[1] = "ID"
colnames(data_vol) = cnames


## min-max scale
# a <- preProcess(data_vol, method=c("range"))
# data_vol_scale <- predict(a, data_vol)

## log transformation 
data_vol_log = data_vol
data_vol_log[,2:17] = log(data_vol[,2:17])

## variables
data_var = dataraw[, c(2,5,6,7,16,22,23,30,31,33,112)]


# thickness
data_thick = dataraw[,c(2,113:183)]
data_thick = data_thick %>%
  select(-"Subjects", -"rh_MeanThickness_thickness", -"lh_MeanThickness_thickness")



## remove "_thickness"
names = gsub("_thickness", "", names(data_thick))

# add level2

# Frontal
# Superior Frontal
# Rostral and Caudal Middle Frontal
# Pars Opercularis, Pars Triangularis, and Pars Orbitalis
# Lateral and Medial Orbitofrontal
# Precentral
# Paracentral
# Frontal Pole
# Rostral Anterior (Frontal)
# Caudal Anterior (Frontal)

subnames_frontal <- c('frontal','pars','precentral', 'paracentral',
                      'rostralanteriorcingulate','caudalanteriorcingulate','insula')
sub_ind_fontal <- grep(paste(subnames_frontal, collapse ='|'), names, ignore.case = TRUE)
names[sub_ind_fontal] = paste(names[sub_ind_fontal], "frontal", sep = '_')


# Parietal
# Superior Parietal
# Inferior Parietal
# Supramarginal
# Postcentral
# Precuneus
# Posterior (Parietal)
# Isthmus (Parieta

subnames_parietal <- c('superiorparietal','inferiorparietal','supramarginal', 'postcentral','precuneus',
                       'posteriorcingulate','isthmuscingulate')
sub_ind_parietal <- grep(paste(subnames_parietal, collapse ='|'), names, ignore.case = TRUE)
names[sub_ind_parietal] = paste(names[sub_ind_parietal], "parietal", sep = '_')


# Temporal
# Superior, Middle, and Inferior Temporal
# Banks of the Superior Temporal Sulcus
# Fusiform
# Transverse Temporal
# Entorhinal
# Temporal Pole
# Parahippocampal
subnames_temporal <- c('temporal', 'bankssts','fusiform','entorhinal','parahippocampal')
sub_ind_temporal <- grep(paste(subnames_temporal, collapse ='|'), names, ignore.case = TRUE)
names[sub_ind_temporal] = paste(names[sub_ind_temporal], "temporal", sep = '_')


# Occipital
# Lateral Occipital
# Lingual
# Cuneus
# Pericalcarine
subnames_occipital <- c('occipital','lingual','Cuneus','pericalcarine')
sub_ind_occipital <- grep(paste(subnames_occipital, collapse ='|'), names, ignore.case = TRUE)
names[sub_ind_occipital] = paste(names[sub_ind_occipital], "occipital", sep = '_')



colnames(data_thick) = names


## correction
colnames(data_thick)[colnames(data_thick) == "lh_precuneus_parietal_occipital"] ="lh_precuneus_parietal"
colnames(data_thick)[colnames(data_thick) == "rh_precuneus_parietal_occipital"] ="rh_precuneus_parietal"

# b <- preProcess(data_thick, method=c("range"))
# data_thick_scale <- predict(b,data_thick)

data_thick_log = data_thick
data_thick_log[,2:69] = log(data_thick[,2:69])


data_thick_log[,2:69] = scaleall(data_thick_log[,2:69])
data_vol_log[,2:17] = scaleall(data_vol_log[,2:17])
# merge two dataset
data_wide = merge(data_var, data_thick_log, by = "ID")
data_wide = merge(data_wide, data_vol_log, by = "ID")




# data_wide = merge(data_var,data_thick, by = "ID")
# data_wide = merge(data_wide,data_vol, by = "ID")

# normalise dataset
data_wide[,12:dim(data_wide)[2]] =  data_wide[,12:dim(data_wide)[2]]

# remove NAs in 2-min
idx = !is.na(data_wide$Flanker) & !is.na(data_wide$X2_min) & !is.na(data_wide$walk_pace )
data_wide = data_wide[idx,]


# # normalize cols
# data_wide[,c(2,4:dim(data_wide)[2])] =  scale(data_wide[,c(2,4:dim(data_wide)[2])])
# write.csv(data_wide,"../data/data_wide_Flanker.csv")

# transfer from wide to long
# data_long = data_wide %>%
#   pivot_longer(cols = lh_bankssts_temporal:rh_VentralDC_subcortical,
#                names_to = "regions", values_to = "zgraymatter")
# 
# data_long$hemisphere = W.strs_selectbetweenpattern(data_long$regions, NULL, '_',1,1)
# data_long$regionlvl1 = W.strs_selectbetweenpattern(data_long$regions, '_', '_',1,-1)
# data_long$regionlvl2 = W.strs_selectbetweenpattern(data_long$regions, '_', NULL,-1,1)
# 
# write.csv(data_long, "../data/data_long_Flanker.csv")


###########################################################################
# load data
data = data_wide
## half participantes
# idx = sample(1:dim(data_50)[1],50,replace = F)
# 
# data_50 = data_wide[idx,]

data_long = data %>%
  pivot_longer(cols = lh_bankssts_temporal:rh_VentralDC_subcortical,
               names_to = "regions", values_to = "graymatter")

data_long$hemisphere = W.strs_selectbetweenpattern(data_long$regions, NULL, '_',1,1)
data_long$regionlvl1 = W.strs_selectbetweenpattern(data_long$regions, '_', '_',1,-1)
data_long$regionlvl2 = W.strs_selectbetweenpattern(data_long$regions, '_', NULL,-1,1)


data_long$regionlvl1b = W.strs_selectbetweenpattern(data_long$regions, NULL, '_',1,-1)
data_long$regionlvl2b = paste(data_long$hemisphere, data_long$regionlvl2, sep = '_')

data = data_long


# factorize variables 
{
  data$regionlvl1 <- as.factor(data$regionlvl1)
  contrasts(data$regionlvl1) <- "contr.sum"
  
  data$regionlvl2 <- as.factor(data$regionlvl2)
  contrasts(data$regionlvl2) <- "contr.sum"
  
  
  data$regionlvl1b <- as.factor(data$regionlvl1b)
  contrasts(data$regionlvl1b) <- "contr.sum"
  
  data$regionlvl2b <- as.factor(data$regionlvl2b)
  contrasts(data$regionlvl2b) <- "contr.sum"
  
  data$hemisphere <- as.factor( data$hemisphere)
  
  contrasts( data$hemisphere) <- "contr.sum"
  
  data$Gender <- as.factor( data$Gender)
  contrasts( data$Gender) <- "contr.sum"
}


saveRDS(data, file = '../data/data_brms_log_normalized.RDS')

hist(data$graymatter)
