

## load dataset
NGA_PUB <-read.csv("C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\NGA_maps_pub.csv")

# State-level ITN access-----------------------------------------------
getJenksBreaks(NGA_PUB$ITN_access_DHS_18_state_precent, 5, subset = NULL)  
#[1]  9.2 18.0 32.9 45.2 68.2
# (to use in legend)

#Assign State-level ITN access classes
NGA_PUB$ITN_access_DHS_18_state_precent_class[NGA_PUB$ITN_access_DHS_18_state_precent > 45.2] <- 1     # classify as rank 1
NGA_PUB$ITN_access_DHS_18_state_precent_class[NGA_PUB$ITN_access_DHS_18_state_precent  <= 45.2 & NGA_PUB$ITN_access_DHS_18_state_precent > 32.9 ] <- 2    # classify as rank 2
NGA_PUB$ITN_access_DHS_18_state_precent_class[NGA_PUB$ITN_access_DHS_18_state_precent <= 32.9 & NGA_PUB$ITN_access_DHS_18_state_precent > 18.0 ] <- 3     # classify as rank 3
NGA_PUB$ITN_access_DHS_18_state_precent_class[NGA_PUB$ITN_access_DHS_18_state_precent  <= 18.0] <- 4 # classify as rank 4

write_xlsx(NGA_PUB,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\NGA_maps_pub.xlsx")

# SES: Percent pop in lowest quntile per state -----------------------------------------------
getJenksBreaks(NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018,5, subset = NULL)  
# [1]  0.0  7.9 24.5 40.8 63.2
# (to use in legend)

#Assign State-level ITN access classes
NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018_class[NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018 > 40.8] <- 4     # classify as rank 4
NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018_class[NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018 <= 40.8 & NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018 > 24.5 ] <- 3   # classify as rank 3
NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018_class[NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018 <= 24.5 & NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018 > 7.9 ] <- 2    # classify as rank 2
NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018_class[NGA_PUB$Perc_pop_lowest_wealth_quintile_DHS_2018 <= 7.9] <- 1 # classify as rank 1

write_xlsx(NGA_PUB,"C:\\Users\\Alyssa\\OneDrive\\Desktop\\Malaria Consortium\\Nigeria data\\NGA_maps_pub.xlsx")
