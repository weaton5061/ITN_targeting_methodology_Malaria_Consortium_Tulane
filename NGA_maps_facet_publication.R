# --------------------------------------------------------------------------
# CREATING MAPS FOR NET TARGET PUBLICATION --------------------------------
# Author: Alyssa Young, Tulane SPHTM --------------------------------------
# Date last modified: 7/22/2021 -------------------------------------------
# --------------------------------------------------------------------------


# Load packages
library(raster)
library(sp)
library(tmap)
library(tmaptools)
library(BAMMtools)

library(readxl)
library(ggplot2)
library(malariaAtlas)
library(exactextractr)
library(maptools)
library(raster)


library(sf)

library("spDataLarge")
library(dplyr)
library(GADMTools)

#library(exactextractr)

library(maptools)
library(readr)
library(foreign)   ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd")
library(reshape2)  ; library(hablar)
library(tidyr)     
library (viridis)
library(data.table)
library(forecast)  ; library(MASS)
library(tseries)   ; library(scales)
library(tsModel)   ; library(extrafont)
library(lmtest)    ; library(tidyverse)
library(stargazer) ; library(RColorBrewer)
library(readxl)    ; library(olsrr)
library(Hmisc)
library(MASS)
library(dplyr)
library(devEMF)
library(padr)
library(zoo)
library(tidyverse)
library(naniar)
library(GGally)
library(cartogram)
library(mgcv)
library(BAMMtools)
library(rgdal)
library(leaflet)
library(arsenal)
require(data.table)
require(RCurl)
require(R.utils)
require(gdalUtils)
require(parallel)
library (tmap)

# Explore color palette ---------------------------------
palette_explorer()

# Get admin boundaries ----------------------------------------------------

adm0.nga <- st_read(dsn = "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\gadm36_NGA_0.shp")
adm1.nga <- st_read(dsn = "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\gadm36_NGA_1.shp")
adm2.nga <- st_read(dsn = "C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\gadm36_NGA_2.shp")

plot(adm0.nga)
plot(adm1.nga)
plot(adm2.nga)
tm_shape(adm2.nga) + tm_polygons("NAME_2") + tm_legend(show=FALSE)

# View adm2.nga as data frame
adm2.nga.df <- as.data.frame(adm2.nga)
adm1.nga.df <- as.data.frame(adm1.nga)


# View adm2.nga.df
View(adm2.nga.df)

### reading in dataset to map
NGA_PUB <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\NGA_maps_pub.csv")
NGA_PUB.df <- as.data.frame(NGA_PUB)
NGA_PUB.spdf <- merge (adm2.nga, NGA_PUB.df)


# --------------------------------------------------------------------------
# OBJECTIVE 1: create one figure of all input map layers used to -------
#create final prioritization map-------------------------------------------
# --------------------------------------------------------------------------

# --------------------------------------------------------------------------
# 1) Create mean annual rainfall map ---------------------------------------
# --------------------------------------------------------------------------


RF_map <- tm_shape(NGA_PUB.spdf) + 
  tm_fill("rf_class", title = "\nMean \nannual \nrainfall (mm)",
          n = 4, style = "cat", 
          palette = "Greens", labels = c("\U2264 91", " 92 - 147", "148 - 207", "> 207"), 
          lwd =0.2, border.col = "grey40") + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.2, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "a", title.size= 1, title.snap.to.legend= TRUE, frame = FALSE, legend.outside = TRUE, 
            legend.position = c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)


# --------------------------------------------------------------------------
# 2) Create temperature suitability map ------------------------------------
# --------------------------------------------------------------------------
# create custom color palette
YlOrBr <- get_brewer_pal("YlOrBr", n = 4)


temp_suit_map <- tm_shape(NGA_PUB.spdf) +
  tm_fill("temp_suit_class", title = "\nP.f.\ntemperature\nsuitability\nindex",
          n = 4, style = "cat",
          palette = YlOrBr, labels = c("> 0.680", "0.571 - 0.680", "0.454 - 0.570", "\U2264  0.453"),
          lwd =0.2, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.2, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "b", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)



# --------------------------------------------------------------------------
# 3) Create COVID case count map -------------------------------------------
# --------------------------------------------------------------------------

### something up with classes- verify final data set

# getJenksBreaks(NGA_PUB.spdf$Covid_case_28_8_20, 4, subset = NULL) #5  1142  5094 18056

YlOrBr <- get_brewer_pal("YlOrBr", n = 4)

covid_map <- tm_shape(NGA_PUB.spdf) + 
  tm_fill("COVID_class_aug_28", title = "COVID-19\n case\n counts", n = 4, style = "cat", 
          palette = "YlOrRd", labels = c("< 1,143", "1,143- 3,090", "3,091- 5,094", "> 5,094"), 
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "c", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 4) Create No. years since last ITN distribution  map ---------------------
# --------------------------------------------------------------------------

ITN_years_map <- tm_shape(NGA_PUB.spdf) + 
  tm_fill("ITN_dist_class", title = "\nYears since\n last ITN\n distribution",
          n = 4, style = "cat", 
          palette = "Blues", labels = c("< 3 years", "3 years",
                                        "5 years", "\u2265 6 years"), 
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "c", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 5) Create PBO map ---------------------
# --------------------------------------------------------------------------

BuGn <- get_brewer_pal("GnBu", n = 2, contrast = c(0, 0.42))


pbo_map<- tm_shape(NGA_PUB.spdf) + 
  tm_fill("PBO_class", title = "\nPBO nets \ndistributed\nin 2019  ", n=2, style = "cat", 
          palette = "GnBu", labels = c("Yes", "No"), 
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "d", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 6a) Create ITN access map v1-----------------------------------------------
# --------------------------------------------------------------------------


RdPu <- get_brewer_pal("RdPu", n = 4, contrast = c(0, 0.42))

ITN_INLA_map <- tm_shape(NGA_PUB.spdf) + 
  tm_fill("ITN_access_INLA_mclass", title = "\nITN Access v1\n(INLA)", n=4, style = "cat", 
          palette = "RdPu", labels = c("\u2265 49%", "35 - 48%", "23 - 34%", "<23%"),
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "j", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 6b) Create ITN availbility map v2-----------------------------------------------
# --------------------------------------------------------------------------


RdPu <- get_brewer_pal("RdPu", n = 4, contrast = c(0, 0.42))

ITN_DHS_map <- tm_shape(NGA_PUB.spdf) + 
  tm_fill("ITN_access_DHS_18_state_precent_class", title = "\nITN Access v2\n(DHS)", n=4, style = "cat", 
          palette = "RdPu", labels = c("> 45.2 %", "33 - 45.2%", "19 - 32.9%", "<18%"),
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") + 
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "k", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)





# --------------------------------------------------------------------------
# 7) Create SMC map --------------------------------------------------------
# --------------------------------------------------------------------------
ltred <- get_brewer_pal("-Reds", n = 3, contrast = c(0.04, 0.44))

SMC_map <-tm_shape(NGA_PUB.spdf) + 
  tm_fill("smc_class", title = "SMC LGAs", n=2, style = "cat", 
          palette = ltred, labels = c("Yes", "No"), 
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "e", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 8) Create Built up presence map ------------------------------------------
# --------------------------------------------------------------------------

# create custom color palette
YlGnBu <- get_brewer_pal("YlGnBu", n = 4, contrast = c(0, 0.42))

SMOD_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Built_class", title = "\nBuilt up\npresence\nindex", n = 4, style = "cat",
          palette = YlGnBu, labels = c(" \U2264 0.0085", "0.051 -  0.0084", "0.76 - 0.05", "> 0.76"),
  lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "f", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)


# --------------------------------------------------------------------------
# 9) Create IDP map --------------------------------------------------------
# --------------------------------------------------------------------------
IDP_map <- tm_shape(NGA_PUB.spdf) + 
  tm_fill("IDP_cat_binary", title = "Internally\ndisplaced\npopulations \n  ", n=2, style = "cat", 
          palette = "Purples", labels = c("No", "Yes"), 
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "g", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 10) Create Malaria prevalence map --------------------------------------------------------
# --------------------------------------------------------------------------
Prev_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Prev_DHS_18_state_class", title = "Malaria \nPrevalence \n6-59 mo", n = 5, style = "cat",
          palette = "Reds", labels = c(" <12", "13 - 22%", "23 - 32%", "33 - 42%", ">42%"),
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) + 
  tm_layout(title= "h", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# 11) Create SES map --------------------------------------------------------
# -------------------------------------------------------------------------- 

BuGn <- get_brewer_pal("GnBu", n = 4, contrast = c(0, 0.50))

SES_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Perc_pop_lowest_wealth_quintile_DHS_2018_class", title = "% Population\nlowest\nquintile", n = 4, style = "cat",
          palette = BuGn, labels = c(" <8% ", "8 - 24.5%", "24.6 - 40.8%", "> 40.8%"),
          lwd =0.5, border.col = "grey40")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "grey40") +
  tm_scale_bar(position = c(0.55, 0.01)) +  
  tm_layout(title= "i", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)


# --------------------------------------------------------------------------
# 12a) Create Weighted Priortization/Malaria Risk map with INLA access layer-------------------
# --------------------------------------------------------------------------
# create custom color palette
magma <- viridisLite::magma(6)
plasma <- viridisLite::plasma(6)
inferno <- viridisLite::inferno(6)
viridis <- viridisLite:: viridis(6)

# to use as standalone map
Final_prioritization_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_score_INLA_access_class", title = "Nigeria \nPriority Areas (v1)", n = 6, style = "cat",
          palette = viridis, labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
            tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
            tm_layout(title= "a", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
                      legend.position =c("RIGHT","BOTTOM"), legend.title.size = 2, legend.text.size= 1, legend.hist.size = 1) +
            tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

## use for tmap_arrange
Final_prioritization_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_score_INLA_access_class", title = "\nNigeria \nPriority Areas (v1)", n = 6, style = "cat",
          palette = magma, labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "m", title.size= 1, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1, legend.text.size= 0.65, legend.hist.size = 0.65) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)


# --------- WITHOUT COVID CASE COUNTS INCLUDED -----------------------------------

# Version 1
Final_prioritization_map <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_score_INLA_noCOVID_class", title = "Nigeria \nLGA Priority \nRankings (v1)", n = 6, style = "cat",
          palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "a", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1.5, legend.text.size= 1, legend.hist.size = 1) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)



# --------------------------------------------------------------------------
# 12b) Create Weighted Priortization/Malaria Risk map with DHS access layer---------------------
# --------------------------------------------------------------------------
# create custom color palette
magma <- viridisLite::magma(6)
plasma <- viridisLite::plasma(6)
inferno <- viridisLite::inferno(6)
viridis <- viridisLite:: viridis(6)
cividis<-viridisLite:: cividis(6)

Final_prioritization_map_2 <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_Score_DHS_access_class_2", title = "Nigeria \nPriority Areas (v2)", n = 6, style = "cat",
          palette = viridis, labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 2, legend.text.size= 1, legend.hist.size = 1) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)


# --------- WITHOUT COVID CASE COUNTS INCLUDED -----------------------------------

Final_prioritization_map_2 <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_Score_DHS_access_noCOVID_class2", title = "Nigeria \nLGA Priority \nRankings (v2)", n = 6, style = "cat",
          palette = viridis, labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 1.5, legend.text.size= 1, legend.hist.size = 1) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# --------------------------------------------------------------------------
# Make tmap_arrange figure -------------------------------------------------
# --------------------------------------------------------------------------

tmap_arrange_1 <- tmap_arrange(RF_map, 
                               temp_suit_map, 
                               ITN_years_map,
                               pbo_map,
                               SMC_map,
                               SMOD_map,
                               IDP_map,
                               Prev_map,
                               SES_map,
                               ITN_INLA_map,
                               ITN_DHS_map,
                               ncol = 2,
                               outer.margins = 0)

# Save

# save 2 columns of tmap_arrange (10 maps) ----------------------------
# tmap_save(tmap_arrange_1, 
#           "tmap_arrange_1.png", width =7,
#           height=7, units ='in', asp = 0)

# tmap_save(tmap_arrange_1, 
#           "tmap_arrange_2.png", width =7,
#           height=7, units ='in', asp = 0)

# tmap_save(tmap_arrange_1, 
#           "tmap_arrange_3.png", width =7,
#           height=7, units ='in', asp = 0)

tmap_save(tmap_arrange_1, 
          "lettertest.png", width =7,
          height=7, units ='in', asp = 0)

tmap_save(tmap_arrange_1, 
          "lettertest2.png", width =7,
          height=7, units ='in', asp = 0)

tmap_save(tmap_arrange_1, 
          "final_series.png", width =7,
          height=7, units ='in', asp = 0)

#### Apply different color scheme to v1 and v2 final priortization maps

Final_prioritization_map_v1 <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_score_INLA_access_class", title = "\nNigeria \nPriority Areas (v1)", n = 6, style = "cat",
          palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "a", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 2, legend.text.size= 1, legend.hist.size = 1) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)

# tmap_save(Final_prioritization_map_v1, 
#           "Final_prior_v1.png", width =7,
#           height=7, units ='in', asp = 0)

Final_prioritization_map_v2 <-tm_shape(NGA_PUB.spdf) +
  tm_fill("Final_Risk_Score_DHS_access_class_2", title = "\nNigeria \nPriority Areas (v2)", n = 6, style = "cat",
          palette = "Blues", labels = c("Low", "Moderate Low", "Moderate", "Moderate High", "High", "Extremely High"), 
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 2, legend.text.size= 1, legend.hist.size = 1) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)


# ---------------------------------------------------------------------------------------------
# 13) Create 2020 distribtuion plan coverage map relative to recommended prioritization areas --
# --------------------------------------------------------------------------------------------
Purples <- get_brewer_pal("Purples", n = 2)


Dist_map_2020 <-tm_shape(NGA_PUB.spdf) +
  tm_fill("ITN_2020_target", title = "LGAs covered under \n2020 ITN campaign", n = 2, style = "cat",
          palette = "Purples", labels = c("No", "Yes"),  
          lwd =0.5, border.col = "gray39")  + 
  tm_shape(adm2.nga)+ tm_borders (lwd = 0.5, col = "gray39") +
  tm_scale_bar(position = c(0.55, 0.01)) + tm_compass(position= c(0.8, 0.2)) +
  tm_layout(title= "b", title.size= 2, title.snap.to.legend= TRUE,frame = FALSE, legend.outside = TRUE,
            legend.position =c("RIGHT","BOTTOM"), legend.title.size = 2.3, legend.text.size= 1.3, legend.hist.size = 1.3) +
  tm_shape(adm1.nga) + tm_borders(col = "gray39", lwd = 1)



## Need to update formatting; lots of whitespace between maps and legends.scale bars
tmap_save(tmap_arrange_2, 
          "Figure_2.png", width =5,
          height=5, units ='in', asp = 0)

tmap_save (Final_prioritization_map3, "Figure2_a.png")
tmap_save (Dist_map_2020, "Figure2_b.png")

Final_prioritization_map2 + Dist_map_2020 + plot_layout (ncol =2, widths- c (1.5, 1))

# # --------------------------------------------------------------------------
# # OBJECTIVE 2: create one figure of all raster input map layers used to ----
# #create ITN availability INLA layer---------------------------------------
# # --------------------------------------------------------------------------
# 
# # --------------------------------------------------------------------------
# # 1. SMOD LAYER------------------------------------------------------------
# # --------------------------------------------------------------------------
# 
# adm1.spdf<-as(adm1.nga, 'Spatial')
# 
# SMOD.NGA.1 <- raster("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_18_7.tif")
# SMOD.NGA.2 <- raster("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_18_8.tif")
# SMOD.NGA.3 <- raster("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_19_7.tif")
# SMOD.NGA.4 <- raster("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\GHS_SMOD_POP2015_GLOBE_R2019A_54009_1K_V2_0_19_8.tif")
# 
# SMOD.NGA.raster <- raster::mosaic(SMOD.NGA.1, SMOD.NGA.2, SMOD.NGA.3, SMOD.NGA.4, fun=mean)
# rm(SMOD.NGA.1, SMOD.NGA.2, SMOD.NGA.3, SMOD.NGA.4)
# 
# 
# newproj <- "+proj=longlat +datum=WGS84"
# SMOD.NGA.raster <- projectRaster(SMOD.NGA.raster, crs=newproj)
# 
# ##--mask and crop
# SMOD.NGA.raster <- mask(x = SMOD.NGA.raster, mask = adm1.spdf)
# SMOD.NGA.raster <- crop(x = SMOD.NGA.raster, y = extent(adm1.spdf))
# 
# # plot raster layer for built population 
# smod_raster <- malariaAtlas::autoplot_MAPraster(SMOD.NGA.raster,shp_df=adm1.spdf, printed=F)
# full_plot <- smod_raster[[1]] + 
#   scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0,start=1.5,r=-1.0,hue=1.5,n=16)), 
#                        name="Urbanicity") + 
#   ggtitle("SMOD") +
#   theme(axis.text=element_blank(),
#         panel.border=element_rect(fill=NA, color="white"))
# print(full_plot)
# ggsave(filename ="C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\SMOD.png", 
#        width =13.35, height=7.5, units ='in', dpi =320)
# rm(p, full_plot)
# 
# 
# # --------------------------------------------------------------------------
# # 2. Improved housing-------------------------------------------------------
# # --------------------------------------------------------------------------
# 
# ImpHsng_2015.raster <- raster("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\ImpHsng_2015_NGA.tif")
# ImpHsng_2015.raster <- mask(x = ImpHsng_2015.raster, mask = adm1.spdf) # mask
# ImpHsng_2015.raster <- crop(x = ImpHsng_2015.raster, y = extent(adm1.spdf)) # crop
# 
# imphsg <- malariaAtlas::autoplot_MAPraster(ImpHsng_2015.raster,shp_df=adm1.spdf, printed=F)
# full_plot <- imphsg[[1]] + 
#   scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0,start=1.5,r=-1.0,hue=1.5,n=16)), 
#                        name="Prevalence") + 
#   ggtitle("Prevalence of Improved Housing") +
#   theme(axis.text=element_blank(),
#         panel.border=element_rect(fill=NA, color="white"))
# print(full_plot)
# 
# ggsave(filename ="C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\ImprvdHsng.png", 
#        width =13.35, height=7.5, units ='in', dpi =320)
# 
# 
# # --------------------------------------------------------------------------
# # 3. Educational attainment-------------------------------------------------
# # --------------------------------------------------------------------------
# 
# # load educational attainment file as raster layer
# EducAttnmnt_2014.raster <- raster("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\EducAttnmt_FemRepAge_2014.tif")
# EducAttnmnt_2014.raster <- mask(x = EducAttnmnt_2014.raster, mask = adm1.spdf) # mask
# EducAttnmnt_2014.raster <- crop(x = EducAttnmnt_2014.raster, y = extent(adm1.spdf)) # crop
# 
# # plot raster layer for Educational Attainment 
# edat <- malariaAtlas::autoplot_MAPraster(EducAttnmnt_2014.raster,shp_df=adm1.spdf, printed=F)
# full_plot <- edat[[1]] + 
#   scale_fill_gradientn(colors = rev(rje::cubeHelix(gamma=1.0,start=1.5,r=-1.0,hue=1.5,n=16)), 
#                        name="Mean Years of Education") + 
#   ggtitle("Educational Attainment: Women 15-49yrs") +
#   theme(axis.text=element_blank(),
#         panel.border=element_rect(fill=NA, color="white"))
# print(full_plot)
# ggsave(filename ="C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\Geospatial\\Raster layers\\EducAttnmnt.png",
#        width =13.35, height=7.5, units ='in', dpi =320)
# rm(p, full_plot)
# 
# 
# #----------------------------------------------------------------------
# # Create as one plot (all three maps)----------------------------------
# #----------------------------------------------------------------------
# 
# 
# tmap_arrange_raster <- tmap_arrange(smod_raster,
#                                imphsg,
#                                edat,
#                                ncol = 2,
#                                nrow=2,
#                                outer.margins = 0)
