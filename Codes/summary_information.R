# summarize coral health data
rm(list = ls()) # remove everything stored in environment

# load libraries
library(tidyverse)
library(vegan)
# library(ggplot2)

# load survey data
surveys <- read.csv("Data/NI_March2020_coral_health_survey_data.csv", head=T, stringsAsFactors=F) 

# remove surveys without genera information
surveys <- subset(surveys, Genus!="") 

# Tables of coral abundances by genera -------------------------------
# table of total genera abundance
genera_table_total <- surveys %>% group_by(Genus) %>% summarize(Abundance = length(Genus), Percent_Total = round(Abundance/nrow(surveys),2))
write.csv(genera_table_total, "Data/Genera_table_total.csv", row.names=F)

# table of genera abundance by bay
genera_table_by_bay <- surveys %>% group_by(Site, Genus) %>% summarize(Abundance = length(Genus)) %>% spread(key=Site, value=Abundance)
genera_table_by_bay[is.na(genera_table_by_bay)] <- 0
write.csv(genera_table_by_bay, "Data/Genera_table_by_bay.csv", row.names=F)

# table of genera abundance by transect
genera_table_by_transect <- surveys %>% group_by(GPS_Name, Genus) %>% summarize(Abundance = length(Genus)) %>% spread(key=GPS_Name, value=Abundance)
genera_table_by_transect[is.na(genera_table_by_transect)] <- 0
write.csv(genera_table_by_transect, "Data/Genera_table_by_transect.csv", row.names=F)

# table of genera abundance by transect
thermal_stress <- surveys %>% summarize(bleaching = sum(!is.na(Percent_bleaching)),
                                        pale = sum(!is.na(Percent_pale)),
                                        fluorescent = sum(!is.na(Percent_fluorescence)),
                                        partial_mortality = sum(!is.na(Percent_partial_mortality))) %>%
  gather()
colnames(thermal_stress) <- c("Other_condition", "Abundance")
  
other_condition_1 <- surveys %>% group_by(Other_condition) %>%
  summarize(Abundance = length(Other_condition))

other_condition_2 <- surveys %>% group_by(Other_condition_2) %>%
  summarize(Abundance = length(Other_condition_2))

colnames(other_condition_2)[1] <- "Other_condition"

conditions <- rbind(thermal_stress, other_condition_1, other_condition_2)

conditions <- conditions %>% group_by(Other_condition) %>% summarize(Abundance = sum(Abundance)) %>% filter(Other_condition != "")

write.csv(conditions, "Data/Conditions.csv", row.names=F)

# species accumulation curve ----------------------------------------
# format data
genera_table_by_transect <- surveys %>% group_by(Site, Transect, Genus) %>% summarize(Abundance = length(Genus)) %>% spread(key=Genus, value=Abundance)
genera_table_by_transect[is.na(genera_table_by_transect)] <- 0

# remove site column
genera_by_transect_wide <- genera_table_by_transect[,3:ncol(genera_table_by_transect)]

# calculate species accumulation curve values
accurve <- specaccum(genera_by_transect_wide, method="random", permutations=100)

# plot species accumulation curve
plot(accurve$sites, accurve$richness, pch=16, type='b', xlab="Number of Sites", ylab="Genera Richness", main="Norfolk Island")