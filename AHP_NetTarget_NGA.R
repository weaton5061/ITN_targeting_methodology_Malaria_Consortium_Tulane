
# OBJECTIVE: obtain weights to assignm to each input factor, using AHP approach

# PROJECT: ITN targeting in Nigeria, Malaria Consortium
# AUTHOR: Alyssa Young, Tulane SPHTM
# Date created: 9/21/21
# Date last modified: 10.7.21

setwd ("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium")

#load relevant packages and libraries
install.packages("ahpsurvey")
library(ahpsurvey)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
library(tidyverse)
library(devtools)
library(knitr)


# import dataset

atts<- c("RFE", "PfTSI", "prev", "ITNAccess", "NoyrsITN", "PBO", "UrbRur", "SMC", "MWI", "IDPs")
AHP.NGA <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\AHP_translated_survey_response.csv")
AHP.NGA2 <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\AHP_translated_survey_response_inverse.csv")
head(AHP.NGA)
head(AHP.NGA.2)
rename(AHP.NGA, RFE_PfTSI=ï..RFE_PfTSI)

summary(AHP.NGA.2)
summary(AHP.NGA)

# CREATING PAIRWISE COMPARISON MATRIX------------------------------------------------------------------------------------------------------------------------------------
# Convert Saatay scale values from negative to positive, convert negative values to reciprocal in pairwaise matrix ------------------------------------------------------------------------
AHP.NGA %>%
ahp.mat (atts=atts, negconvert = TRUE)%>%
head(4)

# compute individual priorties of survey responses, return dtaaframe containing the preferencce weights from survey responses --------------------------------------------------------------
# normalize matrices so that all columns add up to 1, then compute the avete of the row as the preference weights of each attribute

AHP.NGA.2 <- AHP.NGA %>%
  ahp.mat(atts, negconvert = T)

eigentrue <- ahp.indpref(AHP.NGA.2, atts, method = "eigen")

# Calculation of Individual preference weights using arithmetic aggregation and dominant eigenvalue methods
geom <- ahp.indpref(AHP.NGA.2, atts, method = "arithmetic")
error <- data.frame(id = 1:length(AHP.NGA.2), maxdiff = apply(abs(eigentrue - geom), 1, max))
error %>%
ggplot(aes(x = id, y = maxdiff)) +
geom_point() +
geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
geom_hline(yintercept = 0, color = "gray50") +
scale_x_continuous("Respondent ID") +
scale_y_continuous("Maximum difference") +
theme_minimal()

# result: maximum difference betweeen individual priorties of survey respondants eigenvalue and mean aggregation using each method is minimual (max value of 0.014)
# responses therefore consistent

# Calculation of Aggregated preference weights: 1.) compute individual priorties, 2.) aggregate priorities ------------------------------------------------------------------------------
# ahp.aggpref normlaizes matrices so that all of the colums add up to 1 and then computes the averages of the row as the preference weights of each attribute


# obtain weights ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
amean <-ahp.aggpref(AHP.NGA, atts, method = "arithmetic") #method computes individual priorties of each survey respondant
amean

# Results:
# RFE          PfTSI       prev     ITNAccess   NoyrsITN      PBO     UrbRur        SMC        MWI       IDPs 
# 0.03946863 0.09411548 0.12439301 0.04485829 0.03243711 0.09976134 0.13028035 0.10752825 0.10640464 0.22075289 


mean <- AHP.NGA %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic")

sd <- AHP.NGA %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggpref(atts, method = "arithmetic", aggmethod = "sd")

t(data.frame(mean, sd))%>% kable()


#Aggregated Individual judgements
AHP.NGA%>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggjudge(atts, aggmethod = "geometric")

# Below corresponds with table 3 in Bhatt publication - aggregation of indivudal judgements of all survey responders to generate a row-standardized pairwise comparison 
# matrix- allows for comparison of priorties directly based on aggregated pairwise judgments of survey respondants

#               RFE     PfTSI      prev    ITNAccess NoyrsITN     PBO    UrbRur       SMC       MWI      IDPs
# RFE       1.0000000 0.3860974 0.3343702 0.9036020 1.189207 0.5081327 0.4347209 0.4854918 0.3535534 0.1744392
# PfTSI     2.5900201 1.0000000 0.8801117 2.1491399 2.632148 0.9036020 0.4386913 1.0000000 0.8408964 0.4728708
# prev      2.9906976 1.1362194 1.0000000 3.0800703 3.162278 1.1362194 0.9401508 1.0745699 1.0466351 0.4518010
# ITNAccess 1.1066819 0.4653024 0.3246679 1.0000000 1.316074 0.5623413 0.5623413 0.3976354 0.4221068 0.1796521
# NoyrsITN  0.8408964 0.3799178 0.3162278 0.7598357 1.000000 0.4518010 0.4082483 0.3102016 0.2857440 0.1603659
# PBO       1.9679897 1.1066819 0.8801117 1.7782794 2.213364 1.0000000 1.0000000 0.8408964 0.6865890 0.3432945
# UrbRur    2.3003266 2.2795071 1.0636592 1.7782794 2.449490 1.0000000 1.0000000 1.1066819 0.8739351 0.4638596
# SMC       2.0597671 1.0000000 0.9306049 2.5148669 3.223710 1.1892071 0.9036020 1.0000000 0.7259795 0.3021375
# MWI       2.8284271 1.1892071 0.9554428 2.3690686 3.499636 1.4564753 1.1442497 1.3774493 1.0000000 0.3021375
# IDPs      5.7326568 2.1147425 2.2133638 5.5663154 6.235739 2.9129506 2.1558247 3.3097509 3.3097509 1.0000000

# # Measure and visualize consistency----------------------------------------------------------------------------------------------------------------------------------------------------
# # Saaty showed that when CR is higher than 0.1, the choice is deemed to be inconsistnet
# # The following steps are used to visually evaluate consitency of weights featured in line 60
# # RI when 10 attributes are are present is 1.49
# 
weight <- c(3,4,0,0,
            3,4,3,4,
            6,1,-2,-3,
            1,3,0,1,
            3,-3,-3,-1,
            1,0,0,3,
            -1,2,2,3,
            3,6,3,4,
            4,4,6,1,
            1,1,4,-1,
            0,3,1,4, 4)
            

sample_mat <- ahp.mat(t(weight), atts, negconvert = TRUE)
(cr_std <- ahp.cr(sample_mat, atts))

# Return a vector of Consistnecy Ratio (CR)

cr <- AHP.NGA %>%
  ahp.mat(atts, negconvert = T) %>% 
  ahp.cr(atts)
table(cr <= 0.1)

## working with inconsistent and missing data
preference <- t(ahp.indpref(sample_mat, atts, method = "eigen"))
preference


error <- ahp.error(sample_mat, atts, reciprocal = TRUE)

gm_mean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

mat <- AHP.NGA.2 %>%
  ahp.error(atts, reciprocal = TRUE) %>%
  unlist() %>%
  as.numeric() %>%
  array(dim=c(length(atts), length(atts), length(cityahp))) %>%
  apply(c(1,2), gm_mean)

colnames(mat) <- rownames(mat) <- atts



# # ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Trying out canned approach---------------------------------------------------------------------------------------------------------------------------
# https://cran.r-project.org/web/packages/ahpsurvey/vignettes/my-vignette.html

AHP.NGA <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\AHP_translated_survey_response.csv")
atts<- c("RFE", "PfTSI", "prev", "ITNAccess", "NoyrsITN", "PBO", "UrbRur", "SMC", "MWI", "IDPs")

canned <- ahp(df = AHP.NGA, 
              atts = c('RFE', 'PfTSI', 'prev', 'ITNAccess', 'NoyrsITN', 'PBO', 'UrbRur', 'SMC', 'MWI', 'IDPs'), 
              negconvert = FALSE, 
              reciprocal = TRUE,
              method = 'arithmetic', 
              aggmethod = "arithmetic", 
              qt = 0.2,
              censorcr = 0.1,
              agg = TRUE)

head(canned$indpref)
canned$aggpref

#           AggPref    SD.AggPref
# RFE       0.03946863 0.01875183
# PfTSI     0.09411548 0.06536086
# prev      0.12439301 0.09619408
# ITNAccess 0.04485829 0.03047382
# NoyrsITN  0.03243711 0.01146873
# PBO       0.09976134 0.09376347
# UrbRur    0.13028035 0.11902840
# SMC       0.10752825 0.09569371
# MWI       0.10640464 0.06506670
# IDPs      0.22075289 0.06149412


# AHP.NGA2 <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\AHP_translated_survey_response_inverse.csv")
# atts<- c("RFE", "PfTSI", "prev", "ITNAccess", "NoyrsITN", "PBO", "UrbRur", "SMC", "MWI", "IDPs")
# 
# canned <- ahp(df = AHP.NGA2, 
#               atts = c('RFE', 'PfTSI', 'prev', 'ITNAccess', 'NoyrsITN', 'PBO', 'UrbRur', 'SMC', 'MWI', 'IDPs'), 
#               negconvert = TRUE, 
#               reciprocal = TRUE,
#               method = 'arithmetic', 
#               aggmethod = "arithmetic", 
#               qt = 0.2,
#               censorcr = 0.1,
#               agg = TRUE)
# 
# head(canned$indpref)
# canned$aggpref


atts <- c("cult", "fam", "house", "jobs", "trans")
data(city200)
head(city200)



EV_test <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Eigenvector_test.csv")
EV_test <- matrix(c (0.207, 0.069, 0.052, 0.267, 0.089, 0.044, 0.197, 0.098, 0.049), 3, 3, byrow = TRUE)

# # Eigenvector calculations 
# ev <- eigen(EV_test)
# (values <-ev$values)
# vectors <- ev$vectors
# vectors
# 
EV_test2 <- as.matrix(data.frame(c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.052, 0.034), # RFE
                                 c(0.267, 0.089, 0.044, 0.178, 0.178, 0.089, 0.044, 0.044, 0.044, 0.022), # PfTSI
                                 c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # Prev
                                 c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.053, 0.034), # ITNAccess
                                 c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.052, 0.034), # NoyrsITN
                                 c(0.271, 0.090, 0.030, 0.180, 0.180, 0.090, 0.045, 0.045, 0.045, 0.023), # PBO
                                 c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # UrbRur
                                 c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # SMC
                                 c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # MWI
                                 c(0.154, 0.103, 0.077, 0.154, 0.154, 0.103, 0.077, 0.077, 0.077, 0.026))) # IPDs
EV_test2
ev <- eigen(EV_test2)
ev$values
ev$vectors

EV_test3 <- as.matrix(data.frame(c(1.00, 0.33, 0.25, 1.00, 1.00, 0.33, 0.25, 0.25, 0,25, 0.17), # RFE
                                 c(3.00, 1.00, 0.50, 2.00, 2.00, 1.00, 0.50, 0.50, 0.50, 2.50), # PfTSI
                                 c(4.00, 2.00, 1.00, 4.00, 4.00, 2.00, 1.00, 1.00, 1.00, 0.33), # Prev
                                 c(1.00, 0.33, 0.25, 1.00, 1.00, 0.33, 0.25, 0.25, 0,25, 0.17), # ITNAccess
                                 c(1.00, 0.33, 0.25, 1.00, 1.00, 0.33, 0.25, 0.25, 0,25, 0.17), # NoyrsITN
                                 c(3.00, 1.00, 0.33, 2.00, 2.00, 1.00, 0.50, 0.50, 0.50, 0.25), # PBO
                                 c(4.00, 2.00, 1.00, 4.00, 4.00, 2.00, 1.00, 1.00, 1.00, 0.33), # UrbRur
                                 c(4.00, 2.00, 1.00, 4.00, 4.00, 2.00, 1.00, 1.00, 1.00, 0.33), # SMC
                                 c(4.00, 2.00, 1.00, 4.00, 4.00, 2.00, 1.00, 1.00, 1.00, 0.33), # MWI
                                 c(6.00, 4.00, 3.00, 6.00, 6.00, 4.00, 3.00, 3.00, 3.00, 1.00))) # IPDs
EV_test3
ev <- eigen(EV_test3)
ev$values
ev$vectors

A <- matrix(c(13, -4, 2, -4, 11, -2, 2, -2, 8), 3, 3, byrow=TRUE)
A

EV_test4 <-matrix(c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.052, 0.034, 0.267, 0.089, 0.044, 0.178, 0.178, 0.089, 0.044, 0.044, 0.044, 0.022,
                    0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016, 0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.053, 0.034,
                    0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.053, 0.034, 0.271, 0.090, 0.030, 0.180, 0.180, 0.090, 0.045, 0.045, 0.045, 0.023,
                    0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016, 0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016,
                    0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016, 0.154, 0.103, 0.077, 0.154, 0.154, 0.103, 0.077, 0.077, 0.077, 0.026),
                  10,10, byrow=TRUE)

ev <- eigen(EV_test4)
(values <-ev$values)
(vectors <- ev$vectors)
crossprod (vectors)
zapsmall (crossprod(vectors))
library (matlib)
tr(EV_test4)

  

# Trying Tom, Dick and harry approach : https://www.r-bloggers.com/2016/01/analytic-hierarchy-process-ahp-with-the-ahp-package/

devtools::install_github("gluc/ahp", build_vignettes = TRUE)
install.packages("data.tree")
library(ahp)
library(data.tree)


setwd ("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium")
myAhp <- LoadFile("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\MyAHP.txt")
myAHP <- read.delim("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\MyAHP.txt")
Calculate(myAHP)

AHP.NGA2 <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Matrix2.csv")
Calculate(AHP.NGA2)

GetDataFrame(myAHP)



