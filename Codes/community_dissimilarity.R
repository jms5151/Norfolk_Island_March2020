# community ecology analyses --------------------------------------------------
rm(list = ls()) # remove everything stored in environment

# load libraries
library(tidyverse)
library(vegan)
library(gplots)

# load survey data
surveys <- read.csv("Data/NI_March2020_coral_health_survey_data.csv", head=T, stringsAsFactors=F) 

# remove surveys without genera information
surveys <- subset(surveys, Genus!="") 

# genera composition across transects ----------------------------------------
# format survey data
genera_mat <- surveys %>% group_by(GPS_Name, Genus) %>% summarize(Abundance = length(Genus)) %>% spread(key=Genus, value=Abundance)
genera_mat[is.na(genera_mat)] <- 0
genera_mat2 <- genera_mat[,c(2:ncol(genera_mat))]

# dissimilarity matrix where 0 = same exact species composition across sites
BC_dist_matrix <- vegdist(genera_mat2, method="bray")
BC_dist_matrix <- as.matrix(BC_dist_matrix)
rownames(BC_dist_matrix) <- genera_mat$GPS_Name
colnames(BC_dist_matrix) <- genera_mat$GPS_Name

colfunc <- colorRampPalette(c("black", "red"))
heatmap.2(BC_dist_matrix, trace='none', col=colfunc(25), main='Genera composition across transects') 

# NMDS plot
genera_mat2 <- as.matrix(genera_mat2)
rownames(genera_mat2) <- genera_mat$GPS_Name

genera_comp_NMDS = metaMDS(genera_mat2, k=3)
stressplot(genera_comp_NMDS)
plot(genera_comp_NMDS)
orditorp(genera_comp_NMDS, display="sites", cex=1.25, air=0.01)

# thermal stress prevalence across transects --------------------------------
# format survey data
surveys$ThermalStress <- ifelse(!is.na(surveys$Percent_bleaching)|!is.na(surveys$Percent_pale)|!is.na(surveys$Percent_fluorescence),1,0)
TS_mat <- surveys %>% group_by(GPS_Name, Genus) %>% summarize(PropStress = sum(ThermalStress==1)/length(Genus)) %>% spread(key=Genus, value=PropStress)

TS_mat[is.na(TS_mat)] <- 0
TS_mat2 <- TS_mat[,c(2:ncol(TS_mat))]

# dissimilarity matrix where 0 = same exact composition of genera bleaching across sites
TS_dist_matrix <- vegdist(TS_mat2, method="bray")
TS_dist_matrix <- as.matrix(TS_dist_matrix)
rownames(TS_dist_matrix) <- TS_mat$GPS_Name
colnames(TS_dist_matrix) <- TS_mat$GPS_Name

heatmap.2(TS_dist_matrix, trace='none', col=colfunc(25), main='Percent thermal stress across genera & transects') 

# NMDS plot
TS_mat2 <- as.matrix(TS_mat2)
rownames(TS_mat2) <- TS_mat$GPS_Name

TS_comp_NMDS = metaMDS(TS_mat2, k=3)
stressplot(TS_comp_NMDS)
plot(TS_comp_NMDS)
orditorp(TS_comp_NMDS, display="sites", cex=1.25, air=0.01)
