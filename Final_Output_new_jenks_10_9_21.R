

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
# [1] 1.948 3.090 3.822 4.430 5.180

NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access >= 5.18] <- 6 # Extremely High
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access < 5.18 & NGA_PUB$Final_Risk_score_INLA_access > 4.430] <- 5 # High
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 4.430 & NGA_PUB$Final_Risk_score_INLA_access > 3.822] <- 4 # Moderate High
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 3.822 & NGA_PUB$Final_Risk_score_INLA_access > 3.090] <- 3 # Moderate
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 3.090 & NGA_PUB$Final_Risk_score_INLA_access > 1.948] <- 2 # Moderate Low
NGA_PUB$Final_Risk_score_INLA_access_class [NGA_PUB$Final_Risk_score_INLA_access <= 1.948 ] <- 1 # Low


# Final risk categorization version 2.1 ( with Sate-level DHS ITN Access scores)----------------------------------------------------------
getJenksBreaks(NGA_PUB$Final_Risk_Score_DHS_access, 5, subset = NULL) 
#  [1] 1.896 3.428 3.952 4.410 5.078

NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access >= 5.078] <- 6 # Extremely High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access < 5.078 & NGA_PUB$Final_Risk_Score_DHS_access > 4.410] <- 5 # High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 4.410 & NGA_PUB$Final_Risk_Score_DHS_access > 3.952] <- 4 # Moderate High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 3.952 & NGA_PUB$Final_Risk_Score_DHS_access > 3.428] <- 3 # Moderate
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 3.428 & NGA_PUB$Final_Risk_Score_DHS_access > 1.896] <- 2 # Moderate Low
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 1.896] <- 1 # Low

# Final risk categorization version 2.2 ( with Sate-level DHS ITN Access scores, but INLA jenks (line 20))----------------------------------------------------------
# [1] 1.948 3.090 3.822 4.430 5.180

NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access >= 5.18] <- 6 # Extremely High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access < 5.18 & NGA_PUB$Final_Risk_Score_DHS_access > 4.430] <- 5 # High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 4.430 & NGA_PUB$Final_Risk_Score_DHS_access > 3.822] <- 4 # Moderate High
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 3.822 & NGA_PUB$Final_Risk_Score_DHS_access > 3.090] <- 3 # Moderate
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 3.090 & NGA_PUB$Final_Risk_Score_DHS_access > 1.948] <- 2 # Moderate Low
NGA_PUB$Final_Risk_Score_DHS_access_class [NGA_PUB$Final_Risk_Score_DHS_access <= 1.948] <- 1 # Low

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
  tm_fill("Final_Risk_Score_DHS_access_class_DHSjenks", title = "Nigeria \nLGA Priority \nRankings (v2)", n = 6, style = "cat",
          palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 1, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 1.5, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "grey14", lwd = 1)


# # with INLA jenks 
# Final_prioritization_map_3 <-tm_shape(NGA_PUB.spdf) +
#   tm_fill("Final_Risk_Score_DHS_access_class_INLAjenks", title = "Nigeria \nPriority Areas (v2)", n = 6, style = "cat",
#           palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
#           lwd =0.5, border.col = "gray39")  + 
#   tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
#   tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
#   tm_layout(title= "b", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
#             legend.position =c("RIGHT","BOTTOM"), legend.title.size = 2, legend.text.size= 1, legend.hist.size = 1) +
#   tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)



write_xlsx(NGA_PUB,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\NGA_pub_maps_AHP_wt_10_21.xlsx")


tmap_arrange_1 <- tmap_arrange(Final_prioritization_map_INLA,
                               Final_prioritization_map_DHS,
                               ncol = 1,
                               outer.margins = 0)

tmap_save(tmap_arrange_1, 
           "tmap_arrange_1.png", width =7,
           height=7, units ='in', asp = 0)
