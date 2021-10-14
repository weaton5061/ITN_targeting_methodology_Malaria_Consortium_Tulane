

# View adm2.nga as data frame
adm2.nga.df <- as.data.frame(adm2.nga)
adm1.nga.df <- as.data.frame(adm1.nga)


# View adm2.nga.df
View(adm2.nga.df)

### reading in dataset to map
NGA_PUB <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\NGA_pub_maps_AHP_wt_10_21.csv")
NGA_PUB.df <- as.data.frame(NGA_PUB)
NGA_PUB.spdf <- merge (adm2.nga, NGA_PUB.df)

# Assigning new jenks/classes based on updated final prioritizaton scores (with AHP weights)------------------------------------------------------------------------------------------

# Final risk categorization version 1 (with INLA ITN Access scores) -----------------------------------------------------------
getJenksBreaks(NGA_PUB$Final_Risk_score_INLA_access, 5, subset = NULL) 
# [1] 1.37 1.82 2.16 2.54 3.06

NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access >= 3.06] <- 6 # Extremely High
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access < 3.06 & NGA_PUB$Final_Risk_score_INLA_access > 2.54] <- 5 # High
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 2.54 & NGA_PUB$Final_Risk_score_INLA_access > 2.16] <- 4 # Moderate High
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 2.16 & NGA_PUB$Final_Risk_score_INLA_access > 1.82] <- 3 # Moderate
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 1.82 & NGA_PUB$Final_Risk_score_INLA_access > 1.37] <- 2 # Moderate Low
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 1.37 ] <- 1 # Low


# Final risk categorization version 2.1 ( with Sate-level DHS ITN Access scores)----------------------------------------------------------
getJenksBreaks(NGA_PUB$Final_Risk_Score_DHS_access, 5, subset = NULL) 
#  [1] 1.19 1.81 2.16 2.49 2.91

NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access >= 2.91] <- 6 # Extremely High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access < 2.91 & NGA_PUB$Final_Risk_Score_DHS_access > 2.49] <- 5 # High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 2.49 & NGA_PUB$Final_Risk_Score_DHS_access > 2.16] <- 4 # Moderate High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 2.16 & NGA_PUB$Final_Risk_Score_DHS_access > 1.81] <- 3 # Moderate
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 1.81 & NGA_PUB$Final_Risk_Score_DHS_access > 1.19] <- 2 # Moderate Low
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 1.19] <- 1 # Low

# # Final risk categorization version 2.2 ( with Sate-level DHS ITN Access scores, but INLA jenks (line 20))----------------------------------------------------------
# # [1] 1.948 3.090 3.822 4.430 5.180
# 
# NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access >= 5.18] <- 6 # Extremely High
# NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access < 5.18 & NGA_PUB$Final_Risk_Score_DHS_access > 4.430] <- 5 # High
# NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 4.430 & NGA_PUB$Final_Risk_Score_DHS_access > 3.822] <- 4 # Moderate High
# NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 3.822 & NGA_PUB$Final_Risk_Score_DHS_access > 3.090] <- 3 # Moderate
# NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 3.090 & NGA_PUB$Final_Risk_Score_DHS_access > 1.948] <- 2 # Moderate Low
# NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 1.948] <- 1 # Low

# --------------------------------------------------------------------------
# 12a) Create Weighted Priortization/Malaria Risk map with INLA access layer-------------------
# --------------------------------------------------------------------------

# to use as standalone map
Final_prioritization_map_INLA <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_score_INLA_access_class", title = "Nigeria \nLGA Priority \nRankings (v1)", n = 6, style = "cat",
          palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 1, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "a", title.size= 1.5, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "grey14", lwd = 1)


# --------------------------------------------------------------------------
# 12b) Create Weighted Priortization/Malaria Risk map with DHS access layer---------------------
# --------------------------------------------------------------------------

# with DHS jenks
Final_prioritization_map_DHS <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_Score_DHS_access_class", title = "Nigeria \nLGA Priority \nRankings (v2)", n = 6, style = "cat",
          palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 1, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 1.5, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "grey14", lwd = 1)


write_xlsx(NGA_PUB,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\NGA_pub_maps_AHP_wt_10_21.xlsx")


tmap_arrange_1 <- tmap_arrange(Final_prioritization_map_INLA,
                               Final_prioritization_map_DHS,
                               ncol = 1,
                               outer.margins = 0)

tmap_save(tmap_arrange_1, 
           "tmap_arrange_2.png", width =7,
           height=7, units ='in', asp = 0)

# loading new dataset with contribution classes

# creating new indicator to classify factor contribution by categories (1-8)
# 1 = RFE
# 2 = No. years since last ITN distribution
# 3 = ITN Access
# 4 = TSI
# 5 = SMOD (built up area index)
# 6 = ITN distribution and SMOD
# 7 = TSI and SMOD
# 8 = ITN distribution, TSI and SMOD

NGA_PUB$Cont_class [NGA_PUB$RFE == 1] <- 1 
NGA_PUB$Cont_class [NGA_PUB$ITN.dist == 1] <- 2
NGA_PUB$Cont_class [NGA_PUB$ITN.Access == 1] <- 3
NGA_PUB$Cont_class [NGA_PUB$TSI == 1] <- 4
NGA_PUB$Cont_class [NGA_PUB$SMOD == 1] <- 5
NGA_PUB$Cont_class [NGA_PUB$Dist_SMOD ==1] <- 6
NGA_PUB$Cont_class [NGA_PUB$TSI_SMOD == 1] <- 7
NGA_PUB$Cont_class [NGA_PUB$Dist_SMOD_TSI ==1 ] <- 8

NGA_PUB.df <- as.data.frame(NGA_PUB)
NGA_PUB.spdf <- merge (adm2.nga, NGA_PUB.df)
# creating map

Factor_cont_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Cont_class", title = "Distribution of \nLGAs by primay factor \contribution ", n = 8, style = "cat",
          palette = viridis, labels = c("Rainfall", "ITN Distribution", "ITN Access", "p.f. TSI", 
                                        "SMOD", "ITN distribuiton and SMOD", "p.f. TSI and SMOD",
                                        "ITN distribution, SMOD and TSI"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 1, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 1.5, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "grey14", lwd = 1)

