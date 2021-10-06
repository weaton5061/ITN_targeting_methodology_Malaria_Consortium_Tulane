
# OBJECTIVE: 

# PROJECT: ITN targeting in Nigeria, Malaria Consortium
# AUTHOR: Alyssa Young, Tulane SPHTM
# Date created: 9/21/21
# Date last modified: 9/21/21

setwd ("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium")

#load relevant packages and libraries
install.packages("ahpsurvey")
library(ahpsurvey)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(ggplot2)
library(tidyverse)
library(devtools)
#dataset will consist of 20 pair-wise comparisons of 10 indicators 
# since we have already distributed surveys among experets to obtain weights, we will take them mean weight base don survey reponses
# and assign intensity of importance based on Saaty's pari wise comparison table with 9 degrees

# import dataset
AHP.NGA <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\AHP_translated_survey_response.csv")
atts<- c("RFE", "PfTSI", "prev", "ITNAccess", "NoyrsITN", "PBO", "UrbRur", "SMC", "MWI", "IDPs")
data(AHP.NGA)
head(AHP.NGA)

# Convert Saatay scale values from negative to positive, convert negative values to reciprocal in pairwaise matrix ------------------------------------------------------------------------
AHP.NGA %>%
ahp.mat (atts=atts, negconvert = TRUE)%>%
head(3)

# compute individual priorties of survey responses, return dtaaframe containing the preferencce weights from survey responses --------------------------------------------------------------
# normalize matrices so that all columns add up to , then compute the avete of the row as thepreference weights of each attribute
AHP.NGA.ipw <- AHP.NGA %>%
  ahp.mat(atts, negconvert = T)
eigentrue <- ahp.indpref(AHP.NGA.ipw, atts, method = "eigen")

# Calculation of Individual preference weights using arithmetic aggregation and dominant eigenvalue methods
geom <- ahp.indpref(AHP.NGA.ipw, atts, method = "arithmetic")
error <- data.frame(id = 1:length(AHP.NGA.ipw), maxdiff = apply(abs(eigentrue - geom), 1, max))
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
AHP.NGA%>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.aggjudge(atts, aggmethod = "geometric")

# Below corresponds with table 3 in Bhatt publication - aggreateion of indivudal judgements of all survey responders to generate a row-standardized pairwise comparison 
# matrix- allows for comparison of priorties directly based on aggregated pairwise judgments of survey respondants
#               RFE       PfTSI  prev      TNAccess NoyrsITN       PBO
# RFE       1.0000000 0.3021375 0.3593041 0.3343702 0.903602 1.1892071
# PfTSI     3.3097509 1.0000000 0.1744392 0.8801117 2.149140 2.6321480
# prev      2.7831577 5.7326568 1.0000000 0.4728708 3.080070 3.1622777
# ITNAccess 2.9906976 1.1362194 2.1147425 1.0000000 0.451801 1.3160740
# NoyrsITN  1.1066819 0.4653024 0.3246679 2.2133638 1.000000 0.1796521
# PBO       0.8408964 0.3799178 0.3162278 0.7598357 5.566315 1.0000000
# UrbRur    1.9679897 1.1066819 0.8801117 1.7782794 2.213364 6.2357393
# SMC       2.3003266 2.2795071 1.0636592 1.7782794 2.449490 1.0000000
# MWI       2.0597671 1.0000000 0.9306049 2.5148669 3.223710 1.1892071
# IDPs      2.8284271 1.1892071 0.9554428 2.3690686 3.499636 1.4564753

#            UrbRur       SMC       MWI      IDPs
# RFE       0.5081327 0.4347209 0.4854918 0.3535534
# PfTSI     0.9036020 0.4386913 1.0000000 0.8408964
# prev      1.1362194 0.9401508 1.0745699 1.0466351
# ITNAccess 0.5623413 0.5623413 0.3976354 0.4221068
# NoyrsITN  0.4518010 0.4082483 0.3102016 0.2857440
# PBO       0.1603659 1.0000000 0.8408964 0.6865890
# UrbRur    1.0000000 0.3432945 1.1066819 0.8739351
# SMC       2.9129506 1.0000000 0.4638596 0.7259795
# MWI       0.9036020 2.1558247 1.0000000 0.3021375
# IDPs      1.1442497 1.3774493 3.3097509 1.0000000


# obtain weights ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
amean <-ahp.aggpref(AHP.NGA.ipw, atts, method = "arithmetic") #method computes individual priorties of each survey respondant
amean

# Results:
#   RFE      PfTSI       prev        ITNAccess    NoyrsITN        PBO       UrbRur        SMC        MWI        IDPs 
# 0.04209338 0.08688515 0.14711189 0.08609489    0.04546651 0.07030886  0.12897643   0.12994317  0.12186506 0.14125467 


# Measure and visualize consistency----------------------------------------------------------------------------------------------------------------------------------------------------
# Saaty showed that when CR is higher than 0.1, the choice is deemed to be inconsistnet
# The following steps are used to visually evaluate consitency of weights featured in line 60
# RI when 10 attributes are are present is 1.49

weight <- c(5,-3,2,-5,
            -7,-1,-7,
            4,-3,
            -7)
sample_mat <- ahp.mat(t(weight), atts, negconvert = TRUE)

(cr_std <- ahp.cr(sample_mat, atts))
#specifying own random index generated  with ahp.ri to be used with ahp.cr
## Generate a random index with 1000 simulations, 5 dimensions and seed 30000 for reproducibility (seed = 42 by default).
(RI <- ahp.ri(nsims = 1000, dim = 5, seed = 30000))
# [1] 1.115356

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#Use this RI to calculate the consitency ratio instead of the defaut one
ahp.cr(sample_mat, atts, RI)

(cr_std <- ahp.cr(sample_mat, atts))

thres <- 0.1
dict <- c("cult" = "Culture", 
          "fam" = "Family", 
          "house" = "Housing", 
          "jobs" = "Jobs", 
          "trans" = "Transportation")

cr.df <- city200 %>%
  ahp.mat(atts, negconvert = TRUE) %>% 
  ahp.cr(atts) %>% 
  data.frame() %>%
  mutate(rowid = 1:length(cr), cr.dum = as.factor(ifelse(cr <= thres, 1, 0))) %>%
  select(cr.dum, rowid)

city200 %>%
  ahp.mat(atts = atts, negconvert = TRUE) %>% 
  ahp.indpref(atts, method = "eigen") %>% 
  mutate(rowid = 1:nrow(eigentrue)) %>%
  left_join(cr.df, by = 'rowid') %>%
  gather(cult, fam, house, jobs, trans, key = "var", value = "pref") %>%
  ggplot(aes(x = var, y = pref)) + 
  geom_violin(alpha = 0.6, width = 0.8, color = "transparent", fill = "gray") +
  geom_jitter(alpha = 0.6, height = 0, width = 0.1, aes(color = cr.dum)) +
  geom_boxplot(alpha = 0, width = 0.3, color = "#808080") +
  scale_x_discrete("Attribute", label = dict) +
  scale_y_continuous("Weight (dominant eigenvalue)", 
                     labels = scales::percent, 
                     breaks = c(seq(0,0.7,0.1))) +
  guides(color=guide_legend(title=NULL))+
  scale_color_discrete(breaks = c(0,1), 
                       labels = c(paste("CR >", thres), 
                                  paste("CR <", thres))) +
  labs(NULL, caption = paste("n =", nrow(city200), ",", "Mean CR =",
                             round(mean(cr),3)))+
  theme_minimal()

# visualize pairwise comparisons via bar chart
cityahp %>%
  ahp.pwerror(atts) %>% 
  gather(top1, top2, top3, key = "max", value = "pair") %>%
  table() %>%
  as.data.frame() %>%
  ggplot(aes(x = pair, y = Freq, fill = max)) + 
  geom_bar(stat = 'identity') +
  scale_y_continuous("Frequency", breaks = c(seq(0,180,20))) +
  scale_fill_discrete(breaks = c("top1", "top2", "top3"), labels = c("1", "2", "3")) +
  scale_x_discrete("Pair") +
  guides(fill = guide_legend(title="Rank")) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1),
        panel.background = element_rect(fill = NA),
        panel.grid.major.y = element_line(colour = "grey80"),
        panel.grid.major.x = element_blank(),
        panel.ontop = FALSE)




EV_test <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Eigenvector_test.csv")
EV_test <- matrix(c (0.207, 0.069, 0.052, 0.267, 0.089, 0.044, 0.197, 0.098, 0.049), 3, 3, byrow = TRUE)

# # Eigenvector calculations 
# ev <- eigen(EV_test)
# (values <-ev$values)
# vectors <- ev$vectors
# vectors
# 
# EV_test2 <- as.matrix(data.frame(c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.052, 0.034), # RFE
#                                  c(0.267, 0.089, 0.044, 0.178, 0.178, 0.089, 0.044, 0.044, 0.044, 0.022), # PfTSI
#                                  c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # Prev
#                                  c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.053, 0.034), # ITNAccess
#                                  c(0.207, 0.069, 0.052, 0.207, 0.207, 0.069, 0.052, 0.052, 0.052, 0.034), # NoyrsITN
#                                  c(0.271, 0.090, 0.030, 0.180, 0.180, 0.090, 0.045, 0.045, 0.045, 0.023), # PBO
#                                  c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # UrbRur
#                                  c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # SMC
#                                  c(0.197, 0.098, 0.049, 0.197, 0.197, 0.098, 0.049, 0.049, 0.049, 0.016), # MWI
#                                  c(0.154, 0.103, 0.077, 0.154, 0.154, 0.103, 0.077, 0.077, 0.077, 0.026))) # IPDs
# ev <- eigen(EV_test2)
# ev$values
# ev$vectors


# Trying out canned approach

AHP.NGA2 <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Matrix2.csv")

atts<- c("RFE", "PfTSI", "prev", "ITNAccess", "NoyrsITN", "PBO", "UrbRur", "SMC", "MWI", "IDPs")

canned <- ahp(df = AHP.NGA2, 
              atts = c('RFE', 'PfTSI', 'prev', 'ITNAccess', 'NoyrsITN', 'PBO', 'UrbRur', 'SMC', 'MWI', 'IDPs'), 
              negconvert = TRUE, 
              reciprocal = TRUE,
              method = 'arithmetic', 
              aggmethod = "arithmetic", 
              qt = 0.2,
              censorcr = 0.1,
              agg = TRUE)


atts<- c("RFE", "PfTSI", "prev", "ITNAccess", "NoyrsITN", "PBO", "UrbRur", "SMC", "MWI", "IDPs")

canned <- ahp(df = AHP.NGA, 
              atts = c('RFE', 'PfTSI', 'prev', 'ITNAccess', 'NoyrsITN', 'PBO', 'UrbRur', 'SMC', 'MWI', 'IDPs'), 
              negconvert = TRUE, 
              reciprocal = TRUE,
              method = 'arithmetic', 
              aggmethod = "arithmetic", 
              qt = 0.2,
              censorcr = 0.1,
              agg = TRUE)

head(canned$indpref)
